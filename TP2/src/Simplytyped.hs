module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-----------------------
-- conversion
-----------------------

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion (LVar str) = Free (Global str)
conversion (LAbs str t term) = let term' = conversion term 
                                  in (Lam t 
                                      (bound2Brujin (Global str) 0 term'))

conversion (LApp term1 term2) = let 
                                  term1' = conversion term1
                                  term2' = conversion term2
                                in
                                  term1' :@: term2'

conversion (LLet str term1 term2) = let 
                                      term1' = conversion term1
                                      term2' = conversion term2
                                    in
                                      (Let term1' (bound2Brujin 
                                      (Global str) 0 term2')) 



bound2Brujin :: Name -> Int -> Term -> Term
bound2Brujin _ _ t@(Bound j) = t
bound2Brujin idx i t@(Free idx') = if (idx == idx') 
                                     then (Bound i)
                                     else t                                

bound2Brujin idx i (term1 :@: term2) = let
                                         term1' = bound2Brujin idx i term1
                                         term2' = bound2Brujin idx i term2
                                       in
                                         term1' :@: term2'

bound2Brujin idx i (Lam t term) = Lam t (bound2Brujin idx (i+1) term)

bound2Brujin idx i (Let term1 term2) = Let term1 
                                           (bound2Brujin idx (i+1) term2)

----------------------------
--- evaluador de términos
----------------------------

-- substituye una variable por un término en otro término
sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
sub i t (Let term1  term2)    = Let (sub (i + 1) t term1)
                                    (sub (i + 1) t term2)

-- convierte un valor en el término equivalente
quote :: Value -> Term
quote (VLam t f) = Lam t f

-- evalúa un término en un entorno dado
eval :: NameEnv Value Type -> Term -> Value
eval env t@(Bound j) = error "ERROR (eval) - Invalid term"

eval env t@(Free str) = case (inEnv env str) of
      (Just (value, _)) -> value
      (Nothing) -> error "ERROR (eval) - Free variable without value in env"

eval env (u :@: v) = let u' = quote (eval env u)
                         v' = quote (eval env v)
                     in eval env (sub 0 v' u')

eval env (Lam t u) = (VLam t u)
eval env (Let term1 term2) = let term1' = quote (eval env term1)
                             in eval env (sub 0 term1' term2)

inEnv :: NameEnv Value Type -> Name -> Maybe (Value, Type)
inEnv [] name = Nothing
inEnv ((envName, (envValue, envType)) : xs) name = 
                              if (envName == name)
                                then (Just (envValue, envType))
                                else inEnv xs name

----------------------
--- type checker
-----------------------

-- infiere el tipo de un término
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

-- infiere el tipo de un término a partir de un entorno local de variables y un entorno global
infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
infer' c e (Let term1 term2) = infer' c e term1 >>=
                                \t -> infer' (t : c) e term2

