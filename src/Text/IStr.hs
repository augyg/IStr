{-# LANGUAGE TemplateHaskell #-}
module Text.IStr where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse
import Debug.Trace

import qualified Language.Haskell.TH as TH

debug :: QuasiQuoter
debug = QuasiQuoter traceExp undefined undefined undefined

-- | e.g. traceExp "1+x"  ->  [| trace "1+x = $([|1+x|])" (1+x) |]
--   (or something)
traceExp :: String -> Q Exp
traceExp s = [|( trace $(showExp s) $(parseE s) )|]

-- | e.g. showExp "1+x"  ->  [| "1+x" ++ " = " ++ show (1+x) |]
showExp :: String -> Q Exp
showExp s = [|( $(stringE s) ++ " = " ++ show $(parseE s) )|]

-- | e.g. parseE "1+x"  ->  (UInfixE (LitE (IntegerL 1)) (VarE +) (VarE x))
parseE :: String -> Q Exp
parseE = return . either (const undefined) id . parseExp


data StringPart = Literal String | AntiQuote String deriving Show

parseHaskell :: String -> String -> [StringPart]
parseHaskell a []          = [Literal (reverse a)]
parseHaskell a ('\\':x:xs) = parseHaskell (x:a) xs
parseHaskell a ('\\':[])   = parseHaskell ('\\':a) []
parseHaskell a ('}':xs)    = AntiQuote (reverse a) : parseStr [] xs
parseHaskell a (x:xs)      = parseHaskell (x:a) xs

parseStr :: String -> String -> [StringPart]
parseStr a []           = [Literal (reverse a)]
parseStr a ('\\':x:xs)  = parseStr (x:a) xs
parseStr a ('\\':[])    = parseStr ('\\':a) []
parseStr a ('#':'{':xs) = Literal (reverse a) : parseHaskell [] xs
parseStr a (x:xs)       = parseStr (x:a) xs

makeExpr :: [StringPart] -> ExpQ
makeExpr [] = [| "" |]
makeExpr ((Literal a):xs)   = TH.appE [| (++) a |] (makeExpr xs)
makeExpr ((AntiQuote a):xs) = TH.appE [| (++) (trimQuotes (show $(reifyStringToHaskell a))) |] (makeExpr xs)

trimQuotes :: String -> String 
trimQuotes s = reverse $ dropWhile (== '"') $ reverse $ dropWhile (== '"') s

reifyStringToHaskell :: String -> Q Exp
reifyStringToHaskell s = 
    case parseExp s of
        Left s' -> TH.reportError s' >> [| "" |]
        Right e ->  return e

rstrExp :: String -> ExpQ
rstrExp s = makeExpr $ parseStr [] $ filter (/= '\r') s

istr :: QuasiQuoter
istr = QuasiQuoter rstrExp undefined undefined undefined



