{-|
  Copyright   :  (C) 2012-2016, University of Twente,
                     2016-2017, Myrtle Software Ltd,
                     2017-2018, Google Inc.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Create Netlists out of normalized CoreHW Terms
-}

{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

module Clash.Netlist where

import           Control.Exception                (throw)
import           Control.Lens                     ((.=),(^.),_2)
import qualified Control.Lens                     as Lens
import           Control.Monad                    (join)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.State.Strict       (runStateT)
import           Data.Binary.IEEE754              (floatToWord, doubleToWord)
import           Data.Char                        (ord)
import           Data.Either                      (lefts,partitionEithers)
import qualified Data.HashMap.Lazy                as HashMap
import           Data.List                        (elemIndex, sortOn)
import           Data.Maybe                       (catMaybes, listToMaybe, fromMaybe)
import qualified Data.Set                         as Set
import           Data.Primitive.ByteArray         (ByteArray (..))
import qualified Data.Text                        as StrictText
import qualified Data.Vector.Primitive            as PV
import           GHC.Integer.GMP.Internals        (Integer (..), BigNat (..))
import           System.FilePath                  ((</>), (<.>))
import           Text.Read                        (readMaybe)

import           Outputable                       (ppr, showSDocUnsafe)
import           SrcLoc                           (SrcSpan,isGoodSrcSpan,noSrcSpan)

import           Clash.Annotations.BitRepresentation.ClashLib
  (coreToType')
import           Clash.Annotations.BitRepresentation.Internal
  (CustomReprs, DataRepr'(..), ConstrRepr'(..), getDataRepr, getConstrRepr)
import           Clash.Annotations.TopEntity      (TopEntity (..))
import           Clash.Core.DataCon               (DataCon (..))
import           Clash.Core.FreeVars              (typeFreeVars)
import           Clash.Core.Literal               (Literal (..))
import           Clash.Core.Name                  (Name(..))
import           Clash.Core.Pretty                (showPpr)
import           Clash.Core.Term
  (Alt, Pat (..), Term (..))
import qualified Clash.Core.Term                  as Core
import           Clash.Core.Type
  (Type (..), coreView, splitFunTys, splitCoreFunForallTy)
import           Clash.Core.TyCon                 (TyConMap)
import           Clash.Core.Util                  (collectArgs, termType)
import           Clash.Core.Var                   (Id, Var (..))
import           Clash.Core.VarEnv
  (InScopeSet, VarEnv, eltsVarEnv, emptyVarEnv, extendVarEnv, lookupVarEnv, lookupVarEnv',
    mkVarEnv)
import           Clash.Driver.Types               (BindingMap, ClashOpts)
import           Clash.Error
  (ClashValidation, clashFailM, clashSuccessM, clashSuccess, clashFail
  ,accumulateErrors, bindValidationM )
import           Clash.Netlist.BlackBox
import           Clash.Netlist.Id
import           Clash.Netlist.Types              as HW
import           Clash.Netlist.Util
import           Clash.Primitives.Types           as P
import           Clash.Util


-- | Generate a hierarchical netlist out of a set of global binders with
-- @topEntity@ at the top.
genNetlist
  :: HasCallStack
  => Bool
  -- ^ Whether this we're compiling a testbench (suppresses certain warnings)
  -> ClashOpts
  -- ^ Options Clash was called with
  -> CustomReprs
  -- ^ Custom bit representations for certain types
  -> BindingMap
  -- ^ Global binders
  -> InScopeSet
  -- ^ Superset of global bindings
  -> [(Id,Maybe TopEntity,Maybe Id)]
  -- ^ All the TopEntities
  -> CompiledPrimMap
  -- ^ Primitive definitions
  -> TyConMap
  -- ^ TyCon cache
  -> (CustomReprs -> TyConMap -> Bool -> Type -> Maybe (Either String HWType))
  -- ^ Hardcoded Type -> HWType translator
  -> Int
  -- ^ Int/Word/Integer bit-width
  -> (IdType -> Identifier -> Identifier)
  -- ^ valid identifiers
  -> (IdType -> Identifier -> Identifier -> Identifier)
  -- ^ extend valid identifiers
  -> [Identifier]
  -- ^ Seen components
  -> FilePath
  -- ^ HDL dir
  -> (Maybe Identifier,Maybe Identifier)
  -- ^ Component name prefix
  -> Id
  -- ^ Name of the @topEntity@
  -> IO (ClashValidation ([(SrcSpan,[Identifier],Component)],[Identifier]))
genNetlist isTb opts reprs globals is0 tops primMap tcm typeTrans iw mkId extId seen env prefixM topEntity = do
  (resultV, s) <-
    runNetlistMonad
      isTb opts reprs globals is0 (mkTopEntityMap tops)
      primMap tcm typeTrans iw mkId extId seen env prefixM $
        genComponent topEntity

  return $ (const (eltsVarEnv $ _components s, _seenComps s)) <$> resultV
  where
    mkTopEntityMap
      :: [(Id,Maybe TopEntity,Maybe Id)]
      -> VarEnv (Type,Maybe TopEntity)
    mkTopEntityMap =
      mkVarEnv . map (\(a,b,_) -> (a,(varType a,b)))

-- | Run a NetlistMonad action in a given environment
runNetlistMonad
  :: HasCallStack
  => Bool
  -- ^ Whether this we're compiling a testbench (suppresses certain warnings)
  -> ClashOpts
  -- ^ Options Clash was called with
  -> CustomReprs
  -- ^ Custom bit representations for certain types
  -> BindingMap
  -- ^ Global binders
  -> InScopeSet
  -- ^ Superset of global bindings
  -> VarEnv (Type, Maybe TopEntity)
  -- ^ TopEntity annotations
  -> CompiledPrimMap
  -- ^ Primitive Definitions
  -> TyConMap
  -- ^ TyCon cache
  -> (CustomReprs -> TyConMap -> Bool -> Type -> Maybe (Either String HWType))
  -- ^ Hardcode Type -> HWType translator
  -> Int
  -- ^ Int/Word/Integer bit-width
  -> (IdType -> Identifier -> Identifier)
  -- ^ valid identifiers
  -> (IdType -> Identifier -> Identifier -> Identifier)
  -- ^ extend valid identifiers
  -> [Identifier]
  -- ^ Seen components
  -> FilePath
  -- ^ HDL dir
  -> (Maybe Identifier,Maybe Identifier)
  -- ^ Component name prefix
  -> NetlistMonad a
  -- ^ Action to run
  -> IO (a, NetlistState)
runNetlistMonad isTb opts reprs binders is0 tops p tcm typeTrans iw mkId extId seenIds_ env prefixM action
  = runStateT (runNetlist action) initState
  where
    initState =
      NetlistState
        binders 0 emptyVarEnv p typeTrans tcm (StrictText.empty,noSrcSpan) iw mkId
        extId [] seenIds' Set.empty names tops env 0 prefixM reprs is0 opts isTb

    (seenIds',names) = genNames mkId prefixM seenIds_ emptyVarEnv binders

genNames :: (IdType -> Identifier -> Identifier)
         -> (Maybe Identifier,Maybe Identifier)
         -> [Identifier]
         -> VarEnv Identifier
         -> BindingMap
         -> ([Identifier], VarEnv Identifier)
genNames mkId prefixM s0 m0 = foldl go (s0,m0)
  where
    go (s,m) (v,_,_,_) =
      let nm' = genComponentName s mkId prefixM v
          s'  = nm':s
          m'  = extendVarEnv v nm' m
      in (s', m')

-- | Generate a component for a given function (caching)
genComponent
  :: HasCallStack
  => Id
  -- ^ Name of the function
  -> NetlistMonad (ClashValidation (SrcSpan, [Identifier], Component))
genComponent compName = do
  compExprM <- lookupVarEnv compName <$> Lens.use bindings
  case compExprM of
    Nothing -> do
      (_, sp) <- Lens.use curCompNm
      let nm = StrictText.pack (show compName)
      let msg = "No normalized expression found for: " `StrictText.append` nm
      clashFailM (Just sp) ($(curLoc) `StrictText.append` msg)
    Just (_,_,_,expr_) -> do
      bindValidationM (genComponentT compName expr_) $ \comp ->
        clashSuccess <$> makeCachedU compName components (return comp)

-- | Generate a component for a given function
genComponentT
  :: HasCallStack
  => Id
  -- ^ Name of the function
  -> Term
  -- ^ Corresponding term
  -> NetlistMonad (ClashValidation (SrcSpan,[Identifier],Component))
genComponentT compName componentExpr = do
  varCount .= 0
  componentName1 <- (`lookupVarEnv'` compName) <$> Lens.use componentNames
  topEntMM <- fmap snd . lookupVarEnv compName <$> Lens.use topEntityAnns
  prefixM <- Lens.use componentPrefix
  let componentName2 = case (prefixM,join topEntMM) of
                         ((Just p,_),Just ann) -> p `StrictText.append` StrictText.pack ('_':t_name ann)
                         (_,Just ann) -> StrictText.pack (t_name ann)
                         _ -> componentName1
  sp <- ((^. _2) . (`lookupVarEnv'` compName)) <$> Lens.use bindings
  curCompNm .= (componentName2,sp)

  tcm <- Lens.use tcCache

  -- HACK: Determine resulttype of this function by looking at its definition
  -- in topEntityAnns, instead of looking at its last binder (which obscure
  -- any attributes [see: Clash.Annotations.SynthesisAttributes]).
  topEntityTypeM     <- lookupVarEnv compName <$> Lens.use topEntityAnns
  let topEntityTypeM' = snd . splitCoreFunForallTy tcm . fst <$> topEntityTypeM

  seenIds .= []
  (compInps,argWrappers,compOutps,resUnwrappers,binders,resultM) <-
    case splitNormalized tcm componentExpr of
      Right (args, binds, res) -> do
        let varType'   = fromMaybe (varType res) topEntityTypeM'
        mkUniqueNormalized topEntMM ((args, binds, res{varType=varType'}))
      Left err ->
        throw (ClashException sp err)

  netDecls  <- fmap catMaybes . mapM mkNetDecl $ filter (maybe (const True) (/=) resultM . fst) binders
  let declsM = fmap concat <$> accumulateErrors (map (uncurry mkDeclarations) binders)

  bindValidationM declsM $ \decls -> do
    case resultM of
      Just result -> do
        Just (NetDecl' _ rw _ _) <- mkNetDecl . head $ filter ((==result) . fst) binders

        let (compOutps',resUnwrappers') = case compOutps of
              [oport] -> ([(rw,oport)],resUnwrappers)
              _       -> let NetDecl n res resTy = head resUnwrappers
                         in  (map (Wire,) compOutps
                             ,NetDecl' n rw res (Right resTy):tail resUnwrappers
                             )
            component      = Component componentName2 compInps compOutps'
                               (netDecls ++ argWrappers ++ decls ++ resUnwrappers')
        ids <- Lens.use seenIds
        clashSuccessM (sp, ids, component)
      -- No result declaration means that the result is empty, this only happens
      -- when the TopEntity has an empty result. We just create an empty component
      -- in this case.
      Nothing -> do
        let component = Component componentName2 compInps [] (netDecls ++ argWrappers ++ decls)
        ids <- Lens.use seenIds
        clashSuccessM (sp, ids, component)

mkNetDecl :: (Id, Term) -> NetlistMonad (Maybe Declaration)
mkNetDecl (id_,tm) = do
  let typ             = varType id_
  hwTy <- unsafeCoreTypeToHWTypeM $(curLoc) typ
  wr   <- termToWireOrReg tm
  if isVoid hwTy
     then return Nothing
     else return . Just $ NetDecl' (addSrcNote (nameLoc nm))
             wr
             (id2identifier id_)
             (Right hwTy)

  where
    nm = varName id_

    termToWireOrReg :: Term -> NetlistMonad WireOrReg
    termToWireOrReg (Case _ _ (_:_:_)) = return Reg
    termToWireOrReg (collectArgs -> (Prim nm' _,_)) = do
      bbM <- HashMap.lookup nm' <$> Lens.use primitives
      case bbM of
        Just (BlackBox {..}) | outputReg -> return Reg
        _ -> return Wire
    termToWireOrReg _ = return Wire

    addSrcNote loc = if isGoodSrcSpan loc
                        then Just (StrictText.pack (showSDocUnsafe (ppr loc)))
                        else Nothing


isWriteToBiSignalPrimitive :: Term -> Bool
isWriteToBiSignalPrimitive e = case collectArgs e of
  (Prim nm _,_) -> nm == StrictText.pack "Clash.Signal.BiSignal.writeToBiSignal#"
  _             -> False

-- | Generate a list of Declarations for a let-binder, return an empty list
-- if the bound expression is represented by 0 bits
mkDeclarations
  :: HasCallStack
  => Id
  -- ^ LHS of the let-binder
  -> Term
  -- ^ RHS of the let-binder
  -> NetlistMonad (ClashValidation [Declaration])
mkDeclarations bndr e = do
  hty <- unsafeCoreTypeToHWTypeM $(curLoc) (varType bndr)
  if isVoid hty && not (isBiSignalOut hty)
     then clashSuccessM []
     else mkDeclarations' bndr e

-- | Generate a list of Declarations for a let-binder
mkDeclarations'
  :: HasCallStack
  => Id
  -- ^ LHS of the let-binder
  -> Term
  -- ^ RHS of the let-binder
  -> NetlistMonad (ClashValidation [Declaration])
mkDeclarations' bndr (Var v) = mkFunApp bndr v []

mkDeclarations' _ e@(Case _ _ []) = do
  (_,sp) <- Lens.use curCompNm
  clashFailM (Just sp) (StrictText.unwords (
    [ $(curLoc), "Not in normal form: Case-decompositions with an empty list"
    , "of alternatives is not supported by Clash:", StrictText.pack (showPpr e)]
    ))

mkDeclarations' bndr (Case scrut altTy alts@(_:_:_)) =
  mkSelection bndr scrut altTy alts

mkDeclarations' bndr app =
  let (appF,(args,tyArgs)) = second partitionEithers $ collectArgs app
  in case appF of
    Var f
      | null tyArgs -> mkFunApp bndr f args
      | otherwise   -> do
        (_,sp) <- Lens.use curCompNm
        clashFailM (Just sp) (StrictText.unwords (
          [ $(curLoc), "Not in normal form: Var-application with Type"
          , "arguments: \n\n", StrictText.pack (showPpr app)]
          ))
    -- Do not generate any assignments writing to a BiSignalOut, as these
    -- do not have any significance in a HDL. The single exception occurs
    -- when writing to a BiSignal using the primitive 'writeToBiSignal'. In
    -- the generate HDL it will write to an inout port, NOT the variable
    -- having the actual type BiSignalOut.
    _ -> do
      hwTy <- unsafeCoreTypeToHWTypeM $(curLoc) (id2type bndr)
      if isBiSignalOut hwTy && not (isWriteToBiSignalPrimitive app)
         then
          clashSuccessM []
         else do
          let expr = mkExpr False (Right bndr) (varType bndr) app
          bindValidationM expr $ \(exprApp,declsApp) -> do
            let dstId = id2identifier bndr
                assn  = case exprApp of
                          Identifier _ Nothing -> []
                          _ -> [Assignment dstId exprApp]
            clashSuccessM (declsApp ++ assn)

-- | Generate a declaration that selects an alternative based on the value of
-- the scrutinee
mkSelection
  :: HasCallStack
  => Id
  -> Term
  -> Type
  -> [Alt]
  -> NetlistMonad (ClashValidation [Declaration])
mkSelection bndr scrut altTy alts = do
  tcm         <- Lens.use tcCache
  reprs       <- Lens.use customReprs
  let scrutTy = termType tcm scrut
      alts'   = (reorderDefault . reorderCustom tcm reprs scrutTy) alts
  scrutHTy    <- unsafeCoreTypeToHWTypeM $(curLoc) scrutTy
  altHTy      <- unsafeCoreTypeToHWTypeM $(curLoc) altTy
  scrutId     <- extendIdentifier Extended
                   (id2identifier bndr)
                   "_selection"
  (_,sp)      <- Lens.use curCompNm

  let expr0 = mkExpr True (Left scrutId) scrutTy scrut
  bindValidationM expr0 $ \(scrutExpr0, scrutDecls) -> do
    let scrutExpr1 = mkScrutExpr sp scrutHTy (fst (head alts')) scrutExpr0
    bindValidationM (return scrutExpr1) $ \scrutExpr2 -> do
      let exprs0 = accumulateErrors (map (mkCondExpr scrutHTy) alts')
      bindValidationM exprs0 $ \res -> do
        let (exprs1, altsDecls) = second concat (unzip res)
        let condDecl = CondAssignment (id2identifier bndr) altHTy scrutExpr2 scrutHTy exprs1
        clashSuccessM $! scrutDecls ++ altsDecls ++ [condDecl]

  where
    mkCondExpr
      :: HasCallStack
      => HWType
      -> (Pat,Term)
      -> NetlistMonad (ClashValidation ((Maybe HW.Literal,Expr),[Declaration]))
    mkCondExpr scrutHTy (pat,alt) = do
      altId <- extendIdentifier Extended
                 (id2identifier bndr)
                 "_sel_alt"
      bindValidationM (mkExpr False (Left altId) altTy alt) $
        \(altExpr, altDecls) ->
          (fmap (,altDecls)) <$> case pat of
            DefaultPat ->
              clashSuccessM (Nothing,altExpr)
            DataPat dc _ _ ->
              clashSuccessM (Just (dcToLiteral scrutHTy (dcTag dc)),altExpr)
            LitPat  (IntegerLiteral i) ->
              clashSuccessM (Just (NumLit i),altExpr)
            LitPat  (IntLiteral i) ->
              clashSuccessM (Just (NumLit i), altExpr)
            LitPat  (WordLiteral w) ->
              clashSuccessM (Just (NumLit w), altExpr)
            LitPat  (CharLiteral c) ->
              clashSuccessM (Just (NumLit . toInteger $ ord c), altExpr)
            LitPat  (Int64Literal i) ->
              clashSuccessM (Just (NumLit i), altExpr)
            LitPat  (Word64Literal w) ->
              clashSuccessM (Just (NumLit w), altExpr)
            LitPat  (NaturalLiteral n) ->
              clashSuccessM (Just (NumLit n), altExpr)
            _  -> do
              (_,sp) <- Lens.use curCompNm
              clashFailM (Just sp) (StrictText.concat (
                [ $(curLoc), "Not an integer literal in LitPat:\n\n"
                , StrictText.pack (showPpr pat)
                ]))

    mkScrutExpr
      :: HasCallStack
      => SrcSpan
      -> HWType
      -> Pat
      -> Expr
      -> ClashValidation Expr
    mkScrutExpr sp scrutHTy pat scrutE =
      case pat of
        DataPat dc _ _ ->
          let modifier = Just (DC (scrutHTy,dcTag dc - 1)) in
          case scrutE of
            Identifier scrutId Nothing ->
              clashSuccess (Identifier scrutId modifier)
            _ ->
              clashFail (Just sp) (StrictText.unwords (
                [ $(curLoc), "Not in normal form: Not a variable reference or"
                , "primitive as s subject of a case-statement:\n\n"
                , StrictText.pack (show scrutE)
                ]))
        _ ->
          clashSuccess scrutE

-- GHC puts default patterns in the first position, we want them in the
-- last position.
reorderDefault
  :: [(Pat, Term)]
  -> [(Pat, Term)]
reorderDefault ((DefaultPat,e):alts') = alts' ++ [(DefaultPat,e)]
reorderDefault alts'                  = alts'

reorderCustom
  :: TyConMap
  -> CustomReprs
  -> Type
  -> [(Pat, Term)]
  -> [(Pat, Term)]
reorderCustom tcm reprs (coreView tcm -> Just ty) alts =
  reorderCustom tcm reprs ty alts
reorderCustom _tcm reprs (coreToType' -> Right typeName) alts =
  case getDataRepr typeName reprs of
    Just (DataRepr' _name _size _constrReprs) ->
      sortOn (patPos reprs . fst) alts
    Nothing ->
      alts
reorderCustom _tcm _reprs _type alts =
  alts

patPos
  :: CustomReprs
  -> Pat
  -> Int
patPos _reprs DefaultPat = -1
patPos _reprs (LitPat _) = 0
patPos reprs pat@(DataPat dataCon _ _) =
  -- We sort data patterns by their syntactical order
  let name = nameOcc $ dcName dataCon in
  case getConstrRepr name reprs of
    Nothing ->
      -- TODO: err
      error $ $(curLoc) ++ (show pat)
    Just (ConstrRepr' _name n _mask _value _anns) ->
      n


-- | Generate a list of Declarations for a let-binder where the RHS is a function application
mkFunApp
  :: HasCallStack
  => Id
  -- ^ LHS of the let-binder
  -> Id
  -- ^ Name of the applied function
  -> [Term]
  -- ^ Function arguments
  -> NetlistMonad (ClashValidation [Declaration])
mkFunApp dst fun args = do
  topAnns <- Lens.use topEntityAnns
  tcm     <- Lens.use tcCache
  case lookupVarEnv fun topAnns of
    Just (ty,annM)
      | let (fArgTys,fResTy) = splitFunTys tcm ty
      , length fArgTys == length args
      -> do
        let dstId = id2identifier dst
        argHWTys <- mapM (unsafeCoreTypeToHWTypeM $(curLoc)) fArgTys
        -- Filter out the arguments of hwtype `Void` and only translate them
        -- to the intermediate HDL afterwards
        let argsBundled   = zip argHWTys (zip args fArgTys)
            argsFiltered  = filter (not . isVoid . fst) argsBundled
            argsFiltered' = map snd argsFiltered
            hWTysFiltered = filter (not . isVoid) argHWTys

        let exprs0 = map (\(e,t) -> mkExpr False (Left dstId) t e) argsFiltered'
            exprs1 = fmap (second concat . unzip) <$> accumulateErrors exprs0

        bindValidationM exprs1 $ \(argExprs, argDecls) -> do
          dstHWty  <- unsafeCoreTypeToHWTypeM $(curLoc) fResTy
          env  <- Lens.use hdlDir
          mkId <- Lens.use mkIdentifierFn
          prefixM <- Lens.use componentPrefix
          let topName = StrictText.unpack (genTopComponentName mkId prefixM annM fun)
          manFile <- case annM of
            Just _  -> return (env </> topName </> topName <.> "manifest")
            Nothing -> return (env </> topName <.> "manifest")
          Just man <- readMaybe <$> liftIO (readFile manFile)
          instDecls <- mkTopUnWrapper fun annM man (dstId,dstHWty)
                         (zip argExprs hWTysFiltered)
          clashSuccessM (argDecls ++ instDecls)

      -- TODO: User error?
      | otherwise -> error $ $(curLoc) ++ "under-applied TopEntity"
    _ -> do
      normalized <- Lens.use bindings
      case lookupVarEnv fun normalized of
        Just _ -> do
          bindValidationM (preserveVarEnv (genComponent fun)) $
            \(_,_,Component compName compInps co _) -> do
            let argTys = map (termType tcm) args
            argHWTys <- mapM coreTypeToHWTypeM argTys
            -- Filter out the arguments of hwtype `Void` and only translate
            -- them to the intermediate HDL afterwards
            let argsBundled   = zip argHWTys (zip args argTys)
                argsFiltered  = filter (maybe True (not . isVoid) . fst) argsBundled
                argsFiltered' = map snd argsFiltered
                tysFiltered   = map snd argsFiltered'
                compOutp      = snd <$> listToMaybe co
            if length tysFiltered == length compInps
              then do
                let dstId  = nameOcc $ varName dst
                    exprs0 = map (\(e,t) -> mkExpr False (Left dstId) t e) argsFiltered'
                    exprs1 = fmap (second concat . unzip) <$> accumulateErrors exprs0

                bindValidationM exprs1 $ \(argExprs,argDecls) -> do
                  (argExprs',argDecls') <- (second concat . unzip) <$> mapM (toSimpleVar dst) (zip argExprs tysFiltered)
                  let inpAssigns    = zipWith (\(i,t) e -> (Identifier i Nothing,In,t,e)) compInps argExprs'
                      outpAssign    = case compOutp of
                        Nothing -> []
                        Just (id_,hwtype) -> [(Identifier id_ Nothing,Out,hwtype,Identifier dstId Nothing)]
                  instLabel <- extendIdentifier Basic compName (StrictText.pack "_" `StrictText.append` dstId)
                  let instDecl      = InstDecl Entity Nothing compName instLabel (outpAssign ++ inpAssigns)
                  clashSuccessM (argDecls ++ argDecls' ++ [instDecl])
              -- TODO: User error?
              else error $ $(curLoc) ++ "under-applied normalized function"
        Nothing -> case args of
          [] -> do
            let dstId = id2identifier dst
            clashSuccessM [Assignment dstId (Identifier (nameOcc $ varName fun) Nothing)]
          -- TODO: User error?
          _ -> error $ $(curLoc) ++ "Unknown function: " ++ showPpr fun

toSimpleVar
  :: Id
  -> (Expr,Type)
  -> NetlistMonad (Expr,[Declaration])
toSimpleVar _ (e@(Identifier _ _),_) = return (e,[])
toSimpleVar dst (e,ty) = do
  argNm <- extendIdentifier Extended
             (id2identifier dst)
             "_fun_arg"
  argNm' <- mkUniqueIdentifier Extended argNm
  hTy <- unsafeCoreTypeToHWTypeM $(curLoc) ty
  let argDecl         = NetDecl Nothing argNm' hTy
      argAssn         = Assignment argNm' e
  return (Identifier argNm' Nothing,[argDecl,argAssn])

-- | Generate an expression for a term occurring on the RHS of a let-binder
mkExpr
  :: HasCallStack
  => Bool
  -- ^ Treat BlackBox expression as declaration
  -> (Either Identifier Id)
  -- ^ Id to assign the result to
  -> Type
  -- ^ Type of the LHS of the let-binder
  -> Term
  -- ^ Term to convert to an expression
  -> NetlistMonad (ClashValidation (Expr, [Declaration]))
  -- ^ Returned expression and a list of generate BlackBox declarations
mkExpr _ _ _ (Core.Literal l) = do
  iw <- Lens.use intWidth
  case l of
    IntegerLiteral i ->
      clashSuccessM (HW.Literal (Just (Signed iw,iw)) $ NumLit i, [])
    IntLiteral i ->
      clashSuccessM (HW.Literal (Just (Signed iw,iw)) $ NumLit i, [])
    WordLiteral w ->
      clashSuccessM (HW.Literal (Just (Unsigned iw,iw)) $ NumLit w, [])
    Int64Literal i ->
      clashSuccessM (HW.Literal (Just (Signed 64,64)) $ NumLit i, [])
    Word64Literal w ->
      clashSuccessM (HW.Literal (Just (Unsigned 64,64)) $ NumLit w, [])
    CharLiteral c ->
      clashSuccessM (HW.Literal (Just (Unsigned 21,21)) . NumLit . toInteger $ ord c, [])
    FloatLiteral r ->
      let f = fromRational r :: Float
          i = toInteger (floatToWord f)
      in  clashSuccessM (HW.Literal (Just (BitVector 32,32)) (NumLit i), [])
    DoubleLiteral r ->
      let d = fromRational r :: Double
          i = toInteger (doubleToWord d)
      in  clashSuccessM (HW.Literal (Just (BitVector 64,64)) (NumLit i), [])
    NaturalLiteral n ->
      clashSuccessM (HW.Literal (Just (Unsigned iw,iw)) $ NumLit n, [])
    ByteArrayLiteral (PV.Vector _ _ (ByteArray ba)) ->
      clashSuccessM (HW.Literal Nothing (NumLit (Jp# (BN# ba))),[])
    -- TODO: User error?
    _ -> error $ $(curLoc) ++ "not an integer or char literal"

mkExpr bbEasD bndr ty app = do
  let (appF,args) = collectArgs app
      tmArgs      = lefts args
  hwTy <- unsafeCoreTypeToHWTypeM $(curLoc) ty
  (_,sp) <- Lens.use curCompNm
  case appF of
    Data dc ->
      mkDcApplication hwTy bndr dc tmArgs
    Prim nm _ ->
      mkPrimitive False bbEasD bndr nm args ty
    Var f
      | null tmArgs ->
          clashSuccessM (Identifier (nameOcc $ varName f) Nothing, [])
      | otherwise ->
          clashFailM (Just sp) (StrictText.concat
            [ $(curLoc), "Not in normal form: top-level binder in "
            , "argument position:\n\n", StrictText.pack (showPpr app) ])
    Case scrut ty' [alt] ->
      mkProjection bbEasD bndr scrut ty' alt
    _ ->
      clashFailM (Just sp) (StrictText.concat
        [ $(curLoc), "Not in normal form: application of a Let/Lam/Case:\n\n"
        , StrictText.pack (showPpr app) ])

-- | Generate an expression that projects a field out of a data-constructor.
--
-- Works for both product types, as sum-of-product types.
mkProjection
  :: HasCallStack
  => Bool
  -- ^ Projection must bind to a simple variable
  -> Either Identifier Id
  -- ^ The signal to which the projection is (potentially) assigned
  -> Term
  -- ^ The subject/scrutinee of the projection
  -> Type
  -- ^ The type of the result
  -> Alt
  -- ^ The field to be projected
  -> NetlistMonad (ClashValidation (Expr, [Declaration]))
mkProjection mkDec bndr scrut altTy alt@(pat,v) = do
  -- TODO: This function has become a mess. Clean up?
  tcm <- Lens.use tcCache
  let scrutTy = termType tcm scrut
      e = Case scrut scrutTy [alt]
  (_,sp) <- Lens.use curCompNm
  varTm <- case v of
    (Var n) -> return n
    -- TODO: clasherr
    _ -> throw (ClashException sp ($(curLoc) ++
                "Not in normal form: RHS of case-projection is not a variable:\n\n"
                 ++ showPpr e))
  sHwTy <- unsafeCoreTypeToHWTypeM $(curLoc) scrutTy
  vHwTy <- unsafeCoreTypeToHWTypeM $(curLoc) altTy
  let
    selModDecls = do
      scrutNm <-
        either
          return
          (\b -> extendIdentifier Extended (id2identifier b) "_projection")
          bndr

      let expr = mkExpr False (Left scrutNm) scrutTy scrut
      bindValidationM expr $ \(scrutExpr,newDecls) ->
        case scrutExpr of
          Identifier newId modM ->
            clashSuccessM (newId, modM, newDecls)
          _ -> do
            scrutNm' <- mkUniqueIdentifier Extended scrutNm
            let scrutDecl = NetDecl Nothing scrutNm' sHwTy
                scrutAssn = Assignment scrutNm' scrutExpr
            clashSuccessM (scrutNm',Nothing,newDecls ++ [scrutDecl,scrutAssn])

  bindValidationM selModDecls $ \(selId, modM, decls) -> do
    let altVarId = nameOcc (varName varTm)
    modifier <-
      case pat of
        DataPat dc exts tms -> do
          let tmsTys     = map varType tms
              tmsFVs     = concatMap (Lens.toListOf typeFreeVars) tmsTys
              -- TODO: clasherr
              tms'       = if any (`elem` tmsFVs) exts
                              then throw (ClashException sp ($(curLoc) ++ "Not in normal form: Pattern binds existential variables:\n\n" ++ showPpr e))
                              else tms
          argHWTys <- mapM coreTypeToHWTypeM tmsTys
          let tmsBundled   = zip argHWTys tms'
              tmsFiltered  = filter (maybe False (not . isVoid) . fst) tmsBundled
              tmsFiltered' = map snd tmsFiltered
          case elemIndex varTm {varType = altTy} tmsFiltered' of
               Nothing -> pure Nothing
               Just fI
                | sHwTy /= vHwTy -> pure $ nestModifier modM (Just (Indexed (sHwTy,dcTag dc - 1,fI)))
                -- When element and subject have the same HW-type,
                -- then the projections is just the identity
                | otherwise      -> pure $ nestModifier modM (Just (DC (Void Nothing,0)))
        -- TODO: clasherr
        _ -> throw (ClashException sp ($(curLoc) ++ "Not in normal form: Unexpected pattern in case-projection:\n\n" ++ showPpr e))
    let extractExpr = Identifier (maybe altVarId (const selId) modifier) modifier
    case bndr of
      Left scrutNm | mkDec -> do
        scrutNm' <- mkUniqueIdentifier Extended scrutNm
        let scrutDecl = NetDecl Nothing scrutNm' vHwTy
            scrutAssn = Assignment scrutNm' extractExpr
        clashSuccessM (Identifier scrutNm' Nothing,scrutDecl:scrutAssn:decls)
      _ -> clashSuccessM (extractExpr, decls)
  where
    nestModifier Nothing  m          = m
    nestModifier m Nothing           = m
    nestModifier (Just m1) (Just m2) = Just (Nested m1 m2)


-- | Generate an expression for a DataCon application occurring on the RHS of a let-binder
mkDcApplication
    :: HasCallStack
    => HWType
    -- ^ HWType of the LHS of the let-binder
    -> (Either Identifier Id)
    -- ^ Id to assign the result to
    -> DataCon
    -- ^ Applied DataCon
    -> [Term]
    -- ^ DataCon Arguments
    -> NetlistMonad (ClashValidation (Expr,[Declaration]))
    -- ^ Returned expression and a list of generate BlackBox declarations
mkDcApplication dstHType bndr dc args = do
  let dcNm = nameOcc (dcName dc)
  tcm                 <- Lens.use tcCache
  let argTys          = map (termType tcm) args
  argNm <- either return (\b -> extendIdentifier Extended (nameOcc (varName b)) "_dc_arg") bndr
  argHWTys            <- mapM coreTypeToHWTypeM argTys

  -- Filter out the arguments of hwtype `Void` and only translate
  -- them to the intermediate HDL afterwards
  let
    argsBundled =
      zip argHWTys (zip args argTys)
    (hWTysFiltered, argsFiltered) =
      unzip (filter (maybe True (not . isVoid) . fst) argsBundled)

  let
    exprs0 =
      accumulateErrors (map (\(e,t) -> mkExpr False (Left argNm) t e) argsFiltered)
    exprs1 =
      fmap (second concat . unzip) <$> exprs0

  bindValidationM exprs1 $ \(argExprs, argDecls) ->
    fmap (,argDecls) <$> case (hWTysFiltered,argExprs) of
      -- Is the DC just a newtype wrapper?
      ([Just argHwTy],[argExpr]) | argHwTy == dstHType ->
        clashSuccessM (HW.DataCon dstHType (DC (Void Nothing,-1)) [argExpr])
      _ -> case dstHType of
        SP _ dcArgPairs -> do
          let dcI      = dcTag dc - 1
              dcArgs   = snd $ indexNote ($(curLoc) ++ "No DC with tag: " ++ show dcI) dcArgPairs dcI
          case compare (length dcArgs) (length argExprs) of
            EQ -> clashSuccessM (HW.DataCon dstHType (DC (dstHType,dcI)) argExprs)
            LT -> error $ $(curLoc) ++ "Over-applied constructor"
            GT -> error $ $(curLoc) ++ "Under-applied constructor"
        Product _ dcArgs ->
          case compare (length dcArgs) (length argExprs) of
            EQ -> clashSuccessM (HW.DataCon dstHType (DC (dstHType,0)) argExprs)
            LT -> error $ $(curLoc) ++ "Over-applied constructor"
            GT -> error $ $(curLoc) ++ "Under-applied constructor"
        Sum _ _ ->
          clashSuccessM (HW.DataCon dstHType (DC (dstHType,dcTag dc - 1)) [])
        CustomSP _ _ _ dcArgsTups -> do
          -- Safely get item from list, or err with note
          let dcI    = dcTag dc - 1
          let note   = $(curLoc) ++ "No DC with tag: " ++ show dcI
          let argTup = indexNote note dcArgsTups dcI
          let (_, _, dcArgs) = argTup

          case compare (length dcArgs) (length argExprs) of
            EQ -> clashSuccessM (HW.DataCon dstHType (DC (dstHType, dcI)) argExprs)
            LT -> error $ $(curLoc) ++ "Over-applied constructor"
            GT -> error $ $(curLoc) ++ "Under-applied constructor"

        CustomSum _ _ _ _ ->
          clashSuccessM (HW.DataCon dstHType (DC (dstHType, dcTag dc - 1)) [])
        Bool ->
          let dc' = case dcTag dc of
                     1  -> HW.Literal Nothing (BoolLit False)
                     2  -> HW.Literal Nothing (BoolLit True)
                     tg -> error $ $(curLoc) ++ "unknown bool literal: " ++ showPpr dc ++ "(tag: " ++ show tg ++ ")"
          in  clashSuccessM dc'
        Vector 0 _ ->
          clashSuccessM (HW.DataCon dstHType VecAppend [])
        Vector 1 _ ->
          case argExprs of
            [e] -> clashSuccessM (HW.DataCon dstHType VecAppend [e])
            _   -> error $ $(curLoc) ++ "Unexpected number of arguments for `Cons`: " ++ showPpr args
        Vector _ _ ->
          case argExprs of
            [e1,e2] -> clashSuccessM (HW.DataCon dstHType VecAppend [e1,e2])
            _       -> error $ $(curLoc) ++ "Unexpected number of arguments for `Cons`: " ++ showPpr args
        RTree 0 _ ->
          case argExprs of
            [e] -> clashSuccessM (HW.DataCon dstHType RTreeAppend [e])
            _   -> error $ $(curLoc) ++ "Unexpected number of arguments for `LR`: " ++ showPpr args
        RTree _ _ ->
          case argExprs of
            [e1,e2] -> clashSuccessM (HW.DataCon dstHType RTreeAppend [e1,e2])
            _       -> error $ $(curLoc) ++ "Unexpected number of arguments for `BR`: " ++ showPpr args
        String ->
          let dc' = case dcTag dc of
                      1 -> HW.Literal Nothing (StringLit "")
                      _ -> error $ $(curLoc) ++ "mkDcApplication undefined for: " ++ show (dstHType,dc,dcTag dc,args,argHWTys)
          in  clashSuccessM dc'
        Void {} ->
          clashSuccessM (Identifier "__VOID__" Nothing)
        Signed _
          | dcNm == "GHC.Integer.Type.S#"
          -> clashSuccessM (head argExprs)
          | dcNm == "GHC.Integer.Type.Jp#"
          -> clashSuccessM (head argExprs)
          | dcNm == "GHC.Integer.Type.Jn#"
          , HW.Literal Nothing (NumLit i) <- head argExprs
          -> clashSuccessM (HW.Literal Nothing (NumLit (negate i)))
        Unsigned _
          | dcNm == "GHC.Natural.NatS#"
          -> clashSuccessM (head argExprs)
          | dcNm == "GHC.Natural.NatJ#"
          -> clashSuccessM (head argExprs)
        _ ->
          -- TODO: User error?
          error $ $(curLoc) ++ "mkDcApplication undefined for: " ++ show (dstHType,dc,args,argHWTys)
