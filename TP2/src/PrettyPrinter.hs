module PrettyPrinter
  ( printTerm  ,     -- pretty printer para terminos
    printType        -- pretty printer para tipos
  )
where

import  Common
import  Text.PrettyPrint.HughesPJ
import  Prelude hiding ((<>))

-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k         ) = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s

pp ii vs (i :@: c         ) = sep
  [ parensIf (isLam i) (pp ii vs i)
  , nest 1 (parensIf (isLam c || isApp c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c
pp ii vs (Let term1 term2) =
  text "let "
    <> pp (ii + 1) vs term1
    <> text "in "
    <> pp (ii + 1) vs term2
pp ii vs (Zero) = text "0"
pp ii vs (Suc term) = 
  text "suc "
    <> (parensIf (isApp term || isLam term || isSuc term) (pp ii vs term))
pp ii vs (Rec term1 term2 term3) = 
  text "R "
    <> pp ii vs term1
    <> text " "
    <> pp ii vs term2
    <> text " "
    <> pp ii vs term3

isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

isSuc :: Term -> Bool
isSuc (Suc _)   = True
isSuc _         = False

-- pretty-printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]
printType NatT = text "Nat"


isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

fv :: Term -> [String]
fv (Bound _              ) = []
fv (Free  (Global n)     ) = [n]
fv (t   :@: u            ) = fv t ++ fv u
fv (Lam _   u            ) = fv u
fv (Let term1 term2      ) = fv term1 ++ fv term2
fv (Zero                 ) = []
fv (Suc term             ) = fv term
fv (Rec term1 term2 term3) = fv term1 ++ fv term2 ++ fv term3

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

