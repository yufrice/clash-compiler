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
import           Control.Monad.Reader             (runReaderT)
import           Control.Monad.State.Strict       (State, runStateT)
import           Data.Binary.IEEE754              (floatToWord, doubleToWord)
import           Data.Char                        (ord)
import           Data.Either                      (partitionEithers)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HashMapS
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

import           Clash.Annotations.Primitive      (extractPrim)
import           Clash.Annotations.BitRepresentation.ClashLib
  (coreToType')
import           Clash.Annotations.BitRepresentation.Internal
  (CustomReprs, DataRepr'(..), ConstrRepr'(..), getDataRepr, getConstrRepr)
import           Clash.Annotations.TopEntity      (TopEntity (..))
import           Clash.Core.DataCon               (DataCon (..))
import           Clash.Core.Literal               (Literal (..))
import           Clash.Core.Name                  (Name(..))
import           Clash.Core.Pretty                (showPpr)
import           Clash.Core.Term
  (Alt, Pat (..), Term (..), TickInfo (..), collectArgs, collectArgsTicks, collectTicks)
import qualified Clash.Core.Term                  as Core
import           Clash.Core.Type
  (Type (..), coreView1, splitFunTys, splitCoreFunForallTy)
import           Clash.Core.TyCon                 (TyConMap)
import           Clash.Core.Util
  (mkApps, mkTicks, stripTicks, termType)
import           Clash.Core.Var                   (Id, Var (..))
import           Clash.Core.VarEnv
  (VarEnv, eltsVarEnv, emptyInScopeSet, emptyVarEnv, extendVarEnv, lookupVarEnv,
   lookupVarEnv', mkVarEnv)
import           Clash.Driver.Types               (BindingMap, ClashOpts (..))
import           Clash.Netlist.BlackBox
import           Clash.Netlist.Id
import           Clash.Netlist.Types              as HW
import           Clash.Netlist.Util
import           Clash.Primitives.Types           as P
import           Clash.Util


-- | Generate a hierarchical netlist out of a set of global binders with
-- @topEntity@ at the top.
genNetlist
  :: Bool
  -- ^ Whether this we're compiling a testbench (suppresses certain warnings)
  -> ClashOpts
  -- ^ Options Clash was called with
  -> CustomReprs
  -- ^ Custom bit representations for certain types
  -> BindingMap
  -- ^ Global binders
  -> [(Id,Maybe TopEntity,Maybe Id)]
  -- ^ All the TopEntities
  -> CompiledPrimMap
  -- ^ Primitive definitions
  -> TyConMap
  -- ^ TyCon cache
  -> (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -- ^ Hardcoded Type -> HWType translator
  -> Int
  -- ^ Int/Word/Integer bit-width
  -> (IdType -> Identifier -> Identifier)
  -- ^ valid identifiers
  -> (IdType -> Identifier -> Identifier -> Identifier)
  -- ^ extend valid identifiers
  -> Bool
  -- ^ Whether the backend supports ifThenElse expressions
  -> HashMap Identifier Word
  -- ^ Seen components
  -> FilePath
  -- ^ HDL dir
  -> (Maybe Identifier,Maybe Identifier)
  -- ^ Component name prefix
  -> Id
  -- ^ Name of the @topEntity@
  -> IO ([([Bool],SrcSpan,HashMap Identifier Word,Component)],HashMap Identifier Word)
genNetlist isTb opts reprs globals tops primMap tcm typeTrans iw mkId extId ite seen env prefixM topEntity = do
  (_,s) <- runNetlistMonad isTb opts reprs globals (mkTopEntityMap tops)
             primMap tcm typeTrans iw mkId extId ite seen env prefixM $
             genComponent topEntity
  return ( eltsVarEnv $ _components s
         , _seenComps s
         )
  where
    mkTopEntityMap
      :: [(Id,Maybe TopEntity,Maybe Id)]
      -> VarEnv (Type,Maybe TopEntity)
    mkTopEntityMap = mkVarEnv . map (\(a,b,_) -> (a,(varType a,b)))

-- | Run a NetlistMonad action in a given environment
runNetlistMonad
  :: Bool
  -- ^ Whether this we're compiling a testbench (suppresses certain warnings)
  -> ClashOpts
  -- ^ Options Clash was called with
  -> CustomReprs
  -- ^ Custom bit representations for certain types
  -> BindingMap
  -- ^ Global binders
  -> VarEnv (Type, Maybe TopEntity)
  -- ^ TopEntity annotations
  -> CompiledPrimMap
  -- ^ Primitive Definitions
  -> TyConMap
  -- ^ TyCon cache
  -> (CustomReprs -> TyConMap -> Type ->
      State HWMap (Maybe (Either String FilteredHWType)))
  -- ^ Hardcode Type -> HWType translator
  -> Int
  -- ^ Int/Word/Integer bit-width
  -> (IdType -> Identifier -> Identifier)
  -- ^ valid identifiers
  -> (IdType -> Identifier -> Identifier -> Identifier)
  -- ^ extend valid identifiers
  -> Bool
  -- ^ Whether the backend supports ifThenElse expressions
  -> HashMap Identifier Word
  -- ^ Seen components
  -> FilePath
  -- ^ HDL dir
  -> (Maybe Identifier,Maybe Identifier)
  -- ^ Component name prefix
  -> NetlistMonad a
  -- ^ Action to run
  -> IO (a, NetlistState)
runNetlistMonad isTb opts reprs s tops p tcm typeTrans iw mkId extId ite seenIds_ env prefixM
  = flip runReaderT (NetlistEnv "" "" Nothing)
  . flip runStateT s'
  . runNetlist
  where
    s' =
      NetlistState
        s 0 emptyVarEnv p typeTrans tcm (StrictText.empty,noSrcSpan) iw mkId
        extId HashMapS.empty seenIds' Set.empty names tops env 0 prefixM reprs opts isTb ite
        HashMapS.empty

    (seenIds',names) = genNames (opt_newInlineStrat opts) mkId prefixM seenIds_
                                emptyVarEnv s

genNames :: Bool
         -> (IdType -> Identifier -> Identifier)
         -> (Maybe Identifier,Maybe Identifier)
         -> HashMap Identifier Word
         -> VarEnv Identifier
         -> BindingMap
         -> (HashMap Identifier Word, VarEnv Identifier)
genNames newInlineStrat mkId prefixM s0 m0 = foldr go (s0,m0)
  where
    go (v,_,_,_) (s,m) =
      let nm' = genComponentName newInlineStrat s mkId prefixM v
          s'  = HashMapS.insert nm' 0 s
          m'  = extendVarEnv v nm' m
      in (s', m')

-- | Generate a component for a given function (caching)
genComponent
  :: HasCallStack
  => Id
  -- ^ Name of the function
  -> NetlistMonad ([Bool],SrcSpan,HashMap Identifier Word,Component)
genComponent compName = do
  compExprM <- lookupVarEnv compName <$> Lens.use bindings
  case compExprM of
    Nothing -> do
      (_,sp) <- Lens.use curCompNm
      throw (ClashException sp ($(curLoc) ++ "No normalized expression found for: " ++ show compName) Nothing)
    Just (_,_,_,expr_) -> do
      makeCachedU compName components $ genComponentT compName expr_

-- | Generate a component for a given function
genComponentT
  :: HasCallStack
  => Id
  -- ^ Name of the function
  -> Term
  -- ^ Corresponding term
  -> NetlistMonad ([Bool],SrcSpan,HashMap Identifier Word,Component)
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

  seenIds .= HashMapS.empty
  (wereVoids,compInps,argWrappers,compOutps,resUnwrappers,binders,resultM) <-
    case splitNormalized tcm componentExpr of
      Right (args, binds, res) -> do
        let varType'   = fromMaybe (varType res) topEntityTypeM'
        mkUniqueNormalized emptyInScopeSet topEntMM ((args, binds, res{varType=varType'}))
      Left err ->
        throw (ClashException sp err Nothing)

  netDecls <- fmap catMaybes . mapM mkNetDecl $ filter (maybe (const True) (/=) resultM . fst) binders
  decls    <- concat <$> mapM (uncurry mkDeclarations) binders

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
      return (wereVoids, sp, ids, component)
    -- No result declaration means that the result is empty, this only happens
    -- when the TopEntity has an empty result. We just create an empty component
    -- in this case.
    Nothing -> do
      let component = Component componentName2 compInps [] (netDecls ++ argWrappers ++ decls)
      ids <- Lens.use seenIds
      return (wereVoids, sp, ids, component)

mkNetDecl :: (Id, Term) -> NetlistMonad (Maybe Declaration)
mkNetDecl (id_,tm) = do
  let typ             = varType id_
  hwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) typ
  wr   <- termToWireOrReg tm
  if isVoid hwTy
     then return Nothing
     else return . Just $ NetDecl' (addSrcNote sp)
             wr
             (id2identifier id_)
             (Right hwTy)

  where
    nm = varName id_
    sp = case tm of {Tick (SrcSpan s) _ -> s; _ -> nameLoc nm}

    termToWireOrReg :: Term -> NetlistMonad WireOrReg
    termToWireOrReg (stripTicks -> Case scrut _ alts0@(_:_:_)) = do
      tcm <- Lens.use tcCache
      let scrutTy = termType tcm scrut
      scrutHTy <- unsafeCoreTypeToHWTypeM' $(curLoc) scrutTy
      ite <- Lens.use backEndITE
      case iteAlts scrutHTy alts0 of
        Just _ | ite -> return Wire
        _ -> return Reg
    termToWireOrReg (collectArgs -> (Prim nm' _,_)) = do
      bbM <- HashMap.lookup nm' <$> Lens.use primitives
      case bbM of
        Just (extractPrim -> Just BlackBox {..}) | outputReg -> return Reg
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
  -> NetlistMonad [Declaration]
mkDeclarations bndr e = do
  hty <- unsafeCoreTypeToHWTypeM' $(curLoc) (varType bndr)
  if isVoid hty && not (isBiSignalOut hty)
     then return []
     else mkDeclarations' bndr e

-- | Generate a list of Declarations for a let-binder
mkDeclarations'
  :: HasCallStack
  => Id
  -- ^ LHS of the let-binder
  -> Term
  -- ^ RHS of the let-binder
  -> NetlistMonad [Declaration]
mkDeclarations' bndr (collectTicks -> (Var v,ticks)) =
  withTicks ticks $ \tickDecls -> do
  mkFunApp (id2identifier bndr) v [] tickDecls

mkDeclarations' _ e@(collectTicks -> (Case _ _ [],_)) = do
  (_,sp) <- Lens.use curCompNm
  throw $ ClashException
          sp
          ( unwords [ $(curLoc)
                    , "Not in normal form: Case-decompositions with an"
                    , "empty list of alternatives not supported:\n\n"
                    , showPpr e
                    ])
          Nothing

mkDeclarations' bndr (collectTicks -> (Case scrut altTy alts@(_:_:_),ticks)) =
  withTicks ticks $ \tickDecls -> do
  mkSelection (Right bndr) scrut altTy alts tickDecls

mkDeclarations' bndr app =
  let (appF,args0,ticks) = collectArgsTicks app
      (args,tyArgs) = partitionEithers args0
  in  withTicks ticks $ \tickDecls -> do
  case appF of
    Var f
      | null tyArgs -> mkFunApp (id2identifier bndr) f args tickDecls
      | otherwise   -> do
        (_,sp) <- Lens.use curCompNm
        throw (ClashException sp ($(curLoc) ++ "Not in normal form: Var-application with Type arguments:\n\n" ++ showPpr app) Nothing)
    -- Do not generate any assignments writing to a BiSignalOut, as these
    -- do not have any significance in a HDL. The single exception occurs
    -- when writing to a BiSignal using the primitive 'writeToBiSignal'. In
    -- the generate HDL it will write to an inout port, NOT the variable
    -- having the actual type BiSignalOut.
    -- _ | isBiSignalOut (id2type bndr) && (not $ isWriteToBiSignalPrimitive app) ->
    --     return []
    _ -> do
      hwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) (id2type bndr)
      if isBiSignalOut hwTy && not (isWriteToBiSignalPrimitive app)
         then return []
         else do
          (exprApp,declsApp0) <- mkExpr False (Right bndr) (varType bndr) app
          let dstId = id2identifier bndr
              assn  = case exprApp of
                        Identifier _ Nothing -> []
                        _ -> [Assignment dstId exprApp]
              declsApp1 = if null declsApp0 then tickDecls else declsApp0
          return (declsApp1 ++ assn)

-- | Generate a declaration that selects an alternative based on the value of
-- the scrutinee
mkSelection
  :: (Either Identifier Id)
  -> Term
  -> Type
  -> [Alt]
  -> [Declaration]
  -> NetlistMonad [Declaration]
mkSelection bndr scrut altTy alts0 tickDecls = do
  let dstId = either id id2identifier bndr
  tcm <- Lens.use tcCache
  let scrutTy = termType tcm scrut
  scrutHTy <- unsafeCoreTypeToHWTypeM' $(curLoc) scrutTy
  scrutId  <- extendIdentifier Extended dstId "_selection"
  (_,sp) <- Lens.use curCompNm
  ite <- Lens.use backEndITE
  case iteAlts scrutHTy alts0 of
    Just (altT,altF)
      | ite
      -> do
      (scrutExpr,scrutDecls) <- case scrutHTy of
        SP {} -> first (mkScrutExpr sp scrutHTy (fst (last alts0))) <$>
                   mkExpr True (Left scrutId) scrutTy scrut
        _ -> mkExpr False (Left scrutId) scrutTy scrut
      altTId <- extendIdentifier Extended dstId "_sel_alt_t"
      altFId <- extendIdentifier Extended dstId "_sel_alt_f"
      (altTExpr,altTDecls) <- mkExpr False (Left altTId) altTy altT
      (altFExpr,altFDecls) <- mkExpr False (Left altFId) altTy altF
      return $! scrutDecls ++ altTDecls ++ altFDecls ++ tickDecls ++
                [Assignment dstId (IfThenElse scrutExpr altTExpr altFExpr)]
    _ -> do
      reprs <- Lens.use customReprs
      let alts1 = (reorderDefault . reorderCustom tcm reprs scrutTy) alts0
      altHTy                 <- unsafeCoreTypeToHWTypeM' $(curLoc) altTy
      (scrutExpr,scrutDecls) <- first (mkScrutExpr sp scrutHTy (fst (head alts1))) <$>
                                  mkExpr True (Left scrutId) scrutTy scrut
      (exprs,altsDecls)      <- (second concat . unzip) <$> mapM (mkCondExpr scrutHTy) alts1
      return $! scrutDecls ++ altsDecls ++ tickDecls ++ [CondAssignment dstId altHTy scrutExpr scrutHTy exprs]
 where
  mkCondExpr :: HWType -> (Pat,Term) -> NetlistMonad ((Maybe HW.Literal,Expr),[Declaration])
  mkCondExpr scrutHTy (pat,alt) = do
    altId <- extendIdentifier Extended
               (either id id2identifier bndr)
               "_sel_alt"
    (altExpr,altDecls) <- mkExpr False (Left altId) altTy alt
    (,altDecls) <$> case pat of
      DefaultPat           -> return (Nothing,altExpr)
      DataPat dc _ _ -> return (Just (dcToLiteral scrutHTy (dcTag dc)),altExpr)
      LitPat  (IntegerLiteral i) -> return (Just (NumLit i),altExpr)
      LitPat  (IntLiteral i) -> return (Just (NumLit i), altExpr)
      LitPat  (WordLiteral w) -> return (Just (NumLit w), altExpr)
      LitPat  (CharLiteral c) -> return (Just (NumLit . toInteger $ ord c), altExpr)
      LitPat  (Int64Literal i) -> return (Just (NumLit i), altExpr)
      LitPat  (Word64Literal w) -> return (Just (NumLit w), altExpr)
      LitPat  (NaturalLiteral n) -> return (Just (NumLit n), altExpr)
      _  -> do
        (_,sp) <- Lens.use curCompNm
        throw (ClashException sp ($(curLoc) ++ "Not an integer literal in LitPat:\n\n" ++ showPpr pat) Nothing)

  mkScrutExpr :: SrcSpan -> HWType -> Pat -> Expr -> Expr
  mkScrutExpr sp scrutHTy pat scrutE = case pat of
    DataPat dc _ _ -> let modifier = Just (DC (scrutHTy,dcTag dc - 1))
                      in case scrutE of
                          Identifier scrutId Nothing -> Identifier scrutId modifier
                          _ -> throw (ClashException sp ($(curLoc) ++ "Not in normal form: Not a variable reference or primitive as subject of a case-statement:\n\n" ++ show scrutE) Nothing)
    _ -> scrutE

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
reorderCustom tcm reprs (coreView1 tcm -> Just ty) alts =
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
  => Identifier -- ^ LHS of the let-binder
  -> Id -- ^ Name of the applied function
  -> [Term] -- ^ Function arguments
  -> [Declaration] -- ^ Tick declarations
  -> NetlistMonad [Declaration]
mkFunApp dstId fun args tickDecls = do
  topAnns <- Lens.use topEntityAnns
  tcm     <- Lens.use tcCache
  case lookupVarEnv fun topAnns of
    Just (ty,annM)
      | let (fArgTys,fResTy) = splitFunTys tcm ty
      , length fArgTys == length args
      -> do
        argHWTys <- mapM (unsafeCoreTypeToHWTypeM' $(curLoc)) fArgTys
        -- Filter out the arguments of hwtype `Void` and only translate them
        -- to the intermediate HDL afterwards
        let argsBundled   = zip argHWTys (zip args fArgTys)
            argsFiltered  = filter (not . isVoid . fst) argsBundled
            argsFiltered' = map snd argsFiltered
            hWTysFiltered = filter (not . isVoid) argHWTys
        (argExprs,argDecls) <- second concat . unzip <$>
                                 mapM (\(e,t) -> mkExpr False (Left dstId) t e)
                                 argsFiltered'
        dstHWty  <- unsafeCoreTypeToHWTypeM' $(curLoc) fResTy
        env  <- Lens.use hdlDir
        mkId <- Lens.use mkIdentifierFn
        prefixM <- Lens.use componentPrefix
        newInlineStrat <- opt_newInlineStrat <$> Lens.use clashOpts
        let topName = StrictText.unpack
                      (genTopComponentName newInlineStrat mkId prefixM annM fun)
            modName = takeWhile (/= '.')
                                (StrictText.unpack (nameOcc (varName fun)))
        manFile <- case annM of
          Just _  -> return (env </> ".." </> modName </> topName </> topName <.> "manifest")
          Nothing -> return (env </> topName <.> "manifest")
        Just man <- readMaybe <$> liftIO (readFile manFile)
        instDecls <- mkTopUnWrapper fun annM man (dstId,dstHWty)
                       (zip argExprs hWTysFiltered)
                       tickDecls
        return (argDecls ++ instDecls)

      | otherwise -> error $ $(curLoc) ++ "under-applied TopEntity"
    _ -> do
      normalized <- Lens.use bindings
      case lookupVarEnv fun normalized of
        Just _ -> do
          (_,_,_,Component compName compInps co _) <- preserveVarEnv $ genComponent fun
          let argTys = map (termType tcm) args
          argHWTys <- mapM coreTypeToHWTypeM' argTys
          -- Filter out the arguments of hwtype `Void` and only translate
          -- them to the intermediate HDL afterwards
          let argsBundled   = zip argHWTys (zip args argTys)
              argsFiltered  = filter (maybe True (not . isVoid) . fst) argsBundled
              argsFiltered' = map snd argsFiltered
              tysFiltered   = map snd argsFiltered'
              compOutp      = snd <$> listToMaybe co
          if length tysFiltered == length compInps
            then do
              (argExprs,argDecls)   <- fmap (second concat . unzip) $! mapM (\(e,t) -> mkExpr False (Left dstId) t e) argsFiltered'
              (argExprs',argDecls') <- (second concat . unzip) <$> mapM (toSimpleVar dstId) (zip argExprs tysFiltered)
              let inpAssigns    = zipWith (\(i,t) e -> (Identifier i Nothing,In,t,e)) compInps argExprs'
                  outpAssign    = case compOutp of
                    Nothing -> []
                    Just (id_,hwtype) -> [(Identifier id_ Nothing,Out,hwtype,Identifier dstId Nothing)]
              instLabel0 <- extendIdentifier Basic compName (StrictText.pack "_" `StrictText.append` dstId)
              instLabel1 <- fromMaybe instLabel0 <$> Lens.view setName
              instLabel2 <- affixName instLabel1
              instLabel3 <- mkUniqueIdentifier Basic instLabel2
              let instDecl = InstDecl Entity Nothing compName instLabel3 [] (outpAssign ++ inpAssigns)
              return (argDecls ++ argDecls' ++ tickDecls ++ [instDecl])
            else error $ $(curLoc) ++ "under-applied normalized function: " ++ showPpr fun
        Nothing -> case args of
          [] -> return [Assignment dstId (Identifier (nameOcc $ varName fun) Nothing)]
          _ -> error $ $(curLoc) ++ "Unknown function: " ++ showPpr fun

toSimpleVar :: Identifier
            -> (Expr,Type)
            -> NetlistMonad (Expr,[Declaration])
toSimpleVar _ (e@(Identifier _ _),_) = return (e,[])
toSimpleVar dstId (e,ty) = do
  argNm <- extendIdentifier Extended
             dstId
             "_fun_arg"
  argNm' <- mkUniqueIdentifier Extended argNm
  hTy <- unsafeCoreTypeToHWTypeM' $(curLoc) ty
  let argDecl         = NetDecl Nothing argNm' hTy
      argAssn         = Assignment argNm' e
  return (Identifier argNm' Nothing,[argDecl,argAssn])

-- | Generate an expression for a term occurring on the RHS of a let-binder
mkExpr :: HasCallStack
       => Bool -- ^ Treat BlackBox expression as declaration
       -> (Either Identifier Id) -- ^ Id to assign the result to
       -> Type -- ^ Type of the LHS of the let-binder
       -> Term -- ^ Term to convert to an expression
       -> NetlistMonad (Expr,[Declaration]) -- ^ Returned expression and a list of generate BlackBox declarations
mkExpr _ _ _ (stripTicks -> Core.Literal l) = do
  iw <- Lens.use intWidth
  case l of
    IntegerLiteral i -> return (HW.Literal (Just (Signed iw,iw)) $ NumLit i, [])
    IntLiteral i     -> return (HW.Literal (Just (Signed iw,iw)) $ NumLit i, [])
    WordLiteral w    -> return (HW.Literal (Just (Unsigned iw,iw)) $ NumLit w, [])
    Int64Literal i   -> return (HW.Literal (Just (Signed 64,64)) $ NumLit i, [])
    Word64Literal w  -> return (HW.Literal (Just (Unsigned 64,64)) $ NumLit w, [])
    CharLiteral c    -> return (HW.Literal (Just (Unsigned 21,21)) . NumLit . toInteger $ ord c, [])
    FloatLiteral r   -> let f = fromRational r :: Float
                            i = toInteger (floatToWord f)
                        in  return (HW.Literal (Just (BitVector 32,32)) (NumLit i), [])
    DoubleLiteral r  -> let d = fromRational r :: Double
                            i = toInteger (doubleToWord d)
                        in  return (HW.Literal (Just (BitVector 64,64)) (NumLit i), [])
    NaturalLiteral n -> return (HW.Literal (Just (Unsigned iw,iw)) $ NumLit n, [])
    ByteArrayLiteral (PV.Vector _ _ (ByteArray ba)) -> return (HW.Literal Nothing (NumLit (Jp# (BN# ba))),[])
    _ -> error $ $(curLoc) ++ "not an integer or char literal"

mkExpr bbEasD bndr ty app =
 let (appF,args,ticks) = collectArgsTicks app
     (tmArgs,tyArgs) = partitionEithers args
 in  withTicks ticks $ \tickDecls -> do
  hwTy    <- unsafeCoreTypeToHWTypeM' $(curLoc) ty
  (_,sp) <- Lens.use curCompNm
  case appF of
    Data dc -> mkDcApplication hwTy bndr dc tmArgs
    Prim nm _ -> mkPrimitive False bbEasD bndr nm args ty tickDecls
    Var f
      | null tmArgs -> return (Identifier (nameOcc $ varName f) Nothing,[])
      | not (null tyArgs) ->
          throw (ClashException sp ($(curLoc) ++ "Not in normal form: Var-application with Type arguments:\n\n" ++ showPpr app) Nothing)
      | otherwise -> do
          argNm0 <- extendIdentifier Extended (either id id2identifier bndr) "_fun_arg"
          argNm1 <- mkUniqueIdentifier Extended argNm0
          hwTyA  <- unsafeCoreTypeToHWTypeM' $(curLoc) ty
          decls  <- mkFunApp argNm1 f tmArgs tickDecls
          return (Identifier argNm1 Nothing, NetDecl' Nothing Wire argNm1 (Right hwTyA):decls)
    Case scrut ty' [alt] -> mkProjection bbEasD bndr scrut ty' alt
    Case scrut tyA alts -> do
      tcm <- Lens.use tcCache
      let scrutTy = termType tcm scrut
      scrutHTy <- unsafeCoreTypeToHWTypeM' $(curLoc) scrutTy
      ite <- Lens.use backEndITE
      let wr = case iteAlts scrutHTy alts of
                 Just _ | ite -> Wire
                 _ -> Reg
      argNm0 <- extendIdentifier Extended (either id id2identifier bndr) "_sel_arg"
      argNm1 <- mkUniqueIdentifier Extended argNm0
      hwTyA  <- unsafeCoreTypeToHWTypeM' $(curLoc) tyA
      decls  <- mkSelection (Left argNm1) scrut tyA alts tickDecls
      return (Identifier argNm1 Nothing, NetDecl' Nothing wr argNm1 (Right hwTyA):decls)
    Letrec binders body -> do
      netDecls <- fmap catMaybes $ mapM mkNetDecl binders
      decls    <- concat <$> mapM (uncurry mkDeclarations) binders
      (bodyE,bodyDecls) <- mkExpr bbEasD bndr ty (mkApps (mkTicks body ticks) args)
      return (bodyE,netDecls ++ decls ++ bodyDecls)
    _ -> throw (ClashException sp ($(curLoc) ++ "Not in normal form: application of a Lambda-expression\n\n" ++ showPpr app) Nothing)

-- | Generate an expression that projects a field out of a data-constructor.
--
-- Works for both product types, as sum-of-product types.
mkProjection
  :: Bool
  -- ^ Projection must bind to a simple variable
  -> Either Identifier Id
  -- ^ The signal to which the projection is (potentially) assigned
  -> Term
  -- ^ The subject/scrutinee of the projection
  -> Type
  -- ^ The type of the result
  -> Alt
  -- ^ The field to be projected
  -> NetlistMonad (Expr, [Declaration])
mkProjection mkDec bndr scrut altTy alt@(pat,v) = do
  tcm <- Lens.use tcCache
  let scrutTy = termType tcm scrut
      e = Case scrut scrutTy [alt]
  (_,sp) <- Lens.use curCompNm
  varTm <- case v of
    (Var n) -> return n
    _ -> throw (ClashException sp ($(curLoc) ++
                "Not in normal form: RHS of case-projection is not a variable:\n\n"
                 ++ showPpr e) Nothing)
  sHwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) scrutTy
  vHwTy <- unsafeCoreTypeToHWTypeM' $(curLoc) altTy
  (selId,modM,decls) <- do
    scrutNm <- either return
                 (\b -> extendIdentifier Extended
                          (id2identifier b)
                          "_projection")
                 bndr
    (scrutExpr,newDecls) <- mkExpr False (Left scrutNm) scrutTy scrut
    case scrutExpr of
      Identifier newId modM -> return (newId,modM,newDecls)
      _ -> do
        scrutNm' <- mkUniqueIdentifier Extended scrutNm
        let scrutDecl = NetDecl Nothing scrutNm' sHwTy
            scrutAssn = Assignment scrutNm' scrutExpr
        return (scrutNm',Nothing,newDecls ++ [scrutDecl,scrutAssn])

  let altVarId = nameOcc (varName varTm)
  modifier <- case pat of
        DataPat dc exts tms -> do
          let tms' = if bindsExistentials exts tms
                       then throw (ClashException sp ($(curLoc) ++ "Not in normal form: Pattern binds existential variables:\n\n" ++ showPpr e) Nothing)
                       else tms
          argHWTys <- mapM coreTypeToHWTypeM' (map varType tms)
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
        _ -> throw (ClashException sp ($(curLoc) ++ "Not in normal form: Unexpected pattern in case-projection:\n\n" ++ showPpr e) Nothing)
  let extractExpr = Identifier (maybe altVarId (const selId) modifier) modifier
  case bndr of
    Left scrutNm | mkDec -> do
      scrutNm' <- mkUniqueIdentifier Extended scrutNm
      let scrutDecl = NetDecl Nothing scrutNm' vHwTy
          scrutAssn = Assignment scrutNm' extractExpr
      return (Identifier scrutNm' Nothing,scrutDecl:scrutAssn:decls)
    _ -> return (extractExpr,decls)
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
    -> NetlistMonad (Expr,[Declaration])
    -- ^ Returned expression and a list of generate BlackBox declarations
mkDcApplication dstHType bndr dc args = do
  let dcNm = nameOcc (dcName dc)
  tcm                 <- Lens.use tcCache
  let argTys          = map (termType tcm) args
  argNm <- either return (\b -> extendIdentifier Extended (nameOcc (varName b)) "_dc_arg") bndr
  argHWTys            <- mapM coreTypeToHWTypeM' argTys
  -- Filter out the arguments of hwtype `Void` and only translate
  -- them to the intermediate HDL afterwards
  let argsBundled   = zip argHWTys (zip args argTys)
      (hWTysFiltered,argsFiltered) = unzip
        (filter (maybe True (not . isVoid) . fst) argsBundled)
  (argExprs,argDecls) <- fmap (second concat . unzip) $! mapM (\(e,t) -> mkExpr False (Left argNm) t e) argsFiltered
  fmap (,argDecls) $! case (hWTysFiltered,argExprs) of
    -- Is the DC just a newtype wrapper?
    ([Just argHwTy],[argExpr]) | argHwTy == dstHType ->
      return (HW.DataCon dstHType (DC (Void Nothing,-1)) [argExpr])
    _ -> case dstHType of
      SP _ dcArgPairs -> do
        let dcI      = dcTag dc - 1
            dcArgs   = snd $ indexNote ($(curLoc) ++ "No DC with tag: " ++ show dcI) dcArgPairs dcI
        case compare (length dcArgs) (length argExprs) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType,dcI)) argExprs)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"
      Product _ _ dcArgs ->
        case compare (length dcArgs) (length argExprs) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType,0)) argExprs)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"
      Sum _ _ ->
        return (HW.DataCon dstHType (DC (dstHType,dcTag dc - 1)) [])
      CustomSP _ _ _ dcArgsTups -> do
        -- Safely get item from list, or err with note
        let dcI    = dcTag dc - 1
        let note   = $(curLoc) ++ "No DC with tag: " ++ show dcI
        let argTup = indexNote note dcArgsTups dcI
        let (_, _, dcArgs) = argTup

        case compare (length dcArgs) (length argExprs) of
          EQ -> return (HW.DataCon dstHType (DC (dstHType, dcI)) argExprs)
          LT -> error $ $(curLoc) ++ "Over-applied constructor"
          GT -> error $ $(curLoc) ++ "Under-applied constructor"

      CustomSum _ _ _ _ ->
        return (HW.DataCon dstHType (DC (dstHType, dcTag dc - 1)) [])
      Bool ->
        let dc' = case dcTag dc of
                   1  -> HW.Literal Nothing (BoolLit False)
                   2  -> HW.Literal Nothing (BoolLit True)
                   tg -> error $ $(curLoc) ++ "unknown bool literal: " ++ showPpr dc ++ "(tag: " ++ show tg ++ ")"
        in  return dc'
      Vector 0 _ -> return (HW.DataCon dstHType VecAppend [])
      Vector 1 _ -> case argExprs of
                      [e] -> return (HW.DataCon dstHType VecAppend [e])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `Cons`: " ++ showPpr args
      Vector _ _ -> case argExprs of
                      [e1,e2] -> return (HW.DataCon dstHType VecAppend [e1,e2])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `Cons`: " ++ showPpr args
      RTree 0 _ -> case argExprs of
                      [e] -> return (HW.DataCon dstHType RTreeAppend [e])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `LR`: " ++ showPpr args
      RTree _ _ -> case argExprs of
                      [e1,e2] -> return (HW.DataCon dstHType RTreeAppend [e1,e2])
                      _ -> error $ $(curLoc) ++ "Unexpected number of arguments for `BR`: " ++ showPpr args
      String ->
        let dc' = case dcTag dc of
                    1 -> HW.Literal Nothing (StringLit "")
                    _ -> error $ $(curLoc) ++ "mkDcApplication undefined for: " ++ show (dstHType,dc,dcTag dc,args,argHWTys)
        in  return dc'
      Void {} -> return (Identifier "__VOID__" Nothing)
      Signed _
        | dcNm == "GHC.Integer.Type.S#"
        -> pure (head argExprs)
        | dcNm == "GHC.Integer.Type.Jp#"
        -> pure (head argExprs)
        | dcNm == "GHC.Integer.Type.Jn#"
        , HW.Literal Nothing (NumLit i) <- head argExprs
        -> pure (HW.Literal Nothing (NumLit (negate i)))
      Unsigned _
        | dcNm == "GHC.Natural.NatS#"
        -> pure (head argExprs)
        | dcNm == "GHC.Natural.NatJ#"
        -> pure (head argExprs)
--      KnownDomain {} ->
--        return (Identifier "__KNOWNDOMAIN__" Nothing)
--        pure $
--        error $ $(curLoc) ++ "mkDcApplication undefined for KnownDomain. "
--                          ++ "Did a blackbox definition try to render it? "
--                          ++ "Context: \n\n"
--                          ++ "dstHType: " ++ show dstHType ++ "\n\n"
--                          ++ "dc: " ++ show dc ++ "\n\n"
--                          ++ "args: " ++ show args ++ "\n\n"
--                          ++ "argHWTys: " ++ show argHWTys ++ "\n\n"
--                          ++ "Callstack: "
--                          ++ prettyCallStack callStack
      _ ->
        error $ $(curLoc) ++ "mkDcApplication undefined for: " ++ show (dstHType,dc,args,argHWTys)
