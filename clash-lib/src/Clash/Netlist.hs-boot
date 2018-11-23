{-|
  Copyright   :  (C) 2015-2016, University of Twente
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

module Clash.Netlist
  (genComponent
  ,mkExpr
  ,mkDcApplication
  ,mkDeclarations
  ,mkNetDecl
  ,mkProjection
  ,mkSelection
  ) where

import Clash.Core.DataCon   (DataCon)
import Clash.Core.Term      (Alt,LetBinding,Term)
import Clash.Core.Type      (Type)
import Clash.Core.Var       (Id)
import Clash.Netlist.Types  (Expr, HWType, Identifier, NetlistMonad, Component,
                             Declaration)
import Clash.Error          (ClashValidation)
import SrcLoc               (SrcSpan)

import GHC.Stack            (HasCallStack)


genComponent
  :: HasCallStack
  => Id
  -> NetlistMonad (ClashValidation (SrcSpan,[Identifier],Component))

mkExpr
  :: HasCallStack
  => Bool
  -> Either Identifier Id
  -> Type
  -> Term
  -> NetlistMonad (ClashValidation (Expr, [Declaration]))

mkDcApplication
  :: HasCallStack
  => HWType
  -> Either Identifier Id
  -> DataCon
  -> [Term]
  -> NetlistMonad (ClashValidation (Expr, [Declaration]))

mkProjection
  :: HasCallStack
  => Bool
  -> Either Identifier Id
  -> Term
  -> Type
  -> Alt
  -> NetlistMonad (ClashValidation (Expr, [Declaration]))

mkSelection
  :: HasCallStack
  => Id
  -> Term
  -> Type
  -> [Alt]
  -> NetlistMonad (ClashValidation [Declaration])

mkNetDecl
  :: LetBinding
  -> NetlistMonad (Maybe Declaration)

mkDeclarations
  :: HasCallStack
  => Id
  -> Term
  -> NetlistMonad (ClashValidation [Declaration])
