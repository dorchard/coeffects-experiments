{-# LANGUAGE NoMonomorphismRestriction #-}

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts
--import Data.Generics.Uniplate.Operations
import Data.Data.Lens
import Control.Lens
import Data.Maybe
import Control.Monad.Trans.State.Lazy 

transformBiM = transformMOnOf biplate uniplate

foo = do modewl <- readFile "Foo.hs"
         let modeD = defaultParseMode 
         let mode = modeD {extensions = map EnableExtension [DataKinds, TypeOperators]}
         case (parseModuleWithMode mode modewl) of
           ParseOk ast -> let x = transformBiM countVarUse ast
                          in do putStrLn $ show (snd $ runState x [])
                                return ast
           ParseFailed s m -> error $ show s ++ ": " ++ m
         

countVarUse :: Decl -> State [(String, [Int])] Decl
countVarUse d@(FunBind [Match _ (Ident f) pats Nothing rhs (BDecls bs)]) = 
    let vars = map (\p -> case p of 
                                  PVar (Ident v) -> Just v 
                                  _              -> Nothing) pats
        bounds = map (\v -> case v of 
                     Just v -> snd $ runState (do transformBiM (countVars v) rhs
                                                  transformBiM (countVars v) bs) 0
                     Nothing -> -1) vars
    in modify ((f, bounds) :) >> return d
countVarUse x = return x

countVars :: String -> Exp -> State Int Exp
countVars v x@(Var (UnQual (Ident v'))) | v == v' = modify (+1) >> return x
                                        | otherwise = return x
countVars v x = return x

{- 
TypeSig (SrcLoc {srcFilename = "<unknown>.hs", srcLine = 10, srcColumn = 1}) [Ident "g"] (TyInfix (TyCon (UnQual (Ident "Int"))) (UnQual (Symbol "?")) (TyFun (TyPromoted (PromotedInteger 2)) (TyInfix (TyCon (UnQual (Ident "Int"))) (UnQual (Symbol "?")) (TyFun (TyPromoted (PromotedInteger 1)) (TyFun (TyApp (TyCon (UnQual (Ident "Maybe"))) (TyCon (UnQual (Ident "Int")))) (TyCon (UnQual (Ident "Int"))))))))
-}

match (TypeSig loc [Ident sf] 

boundMatch loc var arg m 
   (TyInfix t (UnQual (Symbol "?")) (TyFun (TyPromoted (PromotedInteger n))) =
        if n = m then Nothing
        otherwise = error $ "Resource bound on " ++ (show arg) ++ 
                            "th parameter (variable pattern " ++ (show v) ++ ") is inferred as " ++ (show m) ++ " but is annotated with " ++ (show n)