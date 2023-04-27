{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE UndecidableInstances    #-}

module T18324b where


data L a e = L a e
unLoc :: L a e -> e
unLoc (L _ e)  = e

data B = B

type family Anno a = b 

type family XRec p a = r | r -> a
type instance XRec (GhcPass p) a = L (Anno a) a


data GhcPass (pass :: Pass)
data Pass = Rn

type family IdGhcP (pass :: Pass) where
  IdGhcP 'Rn     = B


type GhcRn = GhcPass 'Rn


data ClsInstDecl pass =
  ClsInstDecl { cid_datafam_insts :: LDataFamInstDecl pass }


-- type LTyFamInstDecl pass = XRec pass (TyFamInstDecl pass)
type LDataFamInstDecl pass = XRec pass ([FamEqn pass (HsDataDefn pass)])
-- type TyFamDefltDecl = TyFamInstDecl

type family IdP p
type instance IdP (GhcPass p) = IdGhcP p

type LIdP p = XRec p (IdP p)

data HsDataDefn pass 

data FamEqn pass rhs
  = FamEqn
       { feqn_tycon  :: LIdP pass
       , feqn_rhs    :: rhs }

fffggg :: ClsInstDecl GhcRn -> [Int]
fffggg ddd = -- let
      do
        FamEqn { feqn_tycon = L _ _
               , feqn_rhs   = _ } {-:: FamEqn GhcRn (HsDataDefn GhcRn)-} <- unLoc $ cid_datafam_insts ddd
        [ 0 ]


