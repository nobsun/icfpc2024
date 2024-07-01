{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}

module HaskellExpr
  ( parseExpr
  , dumpVars

  --
  , Parser, parse, parse', runParser
  --
  , rExpr, rAtom
  , rBool,  rInt, rStr
  , rFix, rUnary, rBinary
  , rArg, rLambdaVars, rVar
  -----
  , raise, token, eof, satisfy
  , readable, char, string
  , spaces, spaces1
  ) where

import qualified Data.ByteString.Char8 as B8
import Data.Map (Map)
import qualified Data.Map as Map

import Imports hiding (And)
import Expr
import ParserLib hiding (Parser, raise, satisfy, token, eof, runParser, parse)
import qualified ParserLib as Lib


data Cxt =
  Cxt
  { varMap   :: Map String Var
  , nextVar  :: Var
  } deriving Show

type CharParser = WParser Char
type Parser = StateT Cxt CharParser

runParser :: Parser a -> Cxt -> String -> Either String ((a, Cxt), String)
runParser p icx input = Lib.runParser (runStateT p icx) input

icontext :: Cxt
icontext = Cxt Map.empty 1

parse' :: Parser a -> String -> Either String ((a, Cxt), String)
parse' p = runParser p icontext

parse :: Parser a -> String -> Either String (a, Cxt)
parse p input = fst <$> parse' p input

dumpVars :: (a, Cxt) -> [(String, Var)]
dumpVars = Map.toList . varMap . snd

---

raise :: String -> Parser a
raise = lift . Lib.raise

token :: Parser Char
token = lift Lib.token

eof :: Parser ()
eof = lift Lib.eof

satisfy :: (Char -> Bool) -> Parser Char
satisfy = lift . Lib.satisfy

readable :: Read a => String -> Parser a
readable tag = lift $ Lib.Parser $ do
  input <- get
  case reads input of
    (x, cs):_  -> put cs $> x
    []         -> raise_ $ "HaskellExpr.readable: " ++ tag ++ ": parse error: '" ++ take 5 input ++ "'..."

char' :: Char -> Parser Char
char' c = satisfy (== c)

space :: Parser ()
space = satisfy (`elem` " \n\t") $> ()

spaces1 :: Parser ()
spaces1 = some space $> ()

spaces :: Parser ()
spaces = many space $> ()

---

char :: Char -> Parser Char
char c = spaces *> char' c

string :: String -> Parser String
string s = spaces *> mapM char' s

---

parseExpr :: String -> Either String (Expr, Cxt)
parseExpr = parse (rExpr <* spaces <* eof)

{- $setup
>>> testp p s = fst <$> parse p s
 -}

rExpr :: Parser Expr
rExpr = asum
    [ rFix
    , rUnary
    , rBinary
    , rIf
    {- , rLambda -}
    , rLambdaVars
    , rAtom
    ]

rAtom :: Parser Expr
rAtom = char '(' *> rExpr <* char ')'
    <|> asum [rBool, rInt, rStr, rVar]

rBool ,rInt ,rStr, rFix, rUnary ,rBinary ,rIf, {- rLambda, -}
  rLambdaVars, rVar :: Parser Expr

{- |
>>> testp rBool "True"
Right (EBool True)
>>> testp rInt "123"
Right (EInt 123)
>>> testp rStr "\" foo bar baz \""
Right (EStr " foo bar baz ")
 -}
rBool = EBool <$> readable @Bool "EBool"
rInt  = EInt  <$> readable @Integer "EInt"
rStr  = EStr . B8.pack <$> readable @String "EStr"

appExpr :: Expr' a -> Expr' a -> Expr' a
appExpr e1 e2 = EBinary ApplyLazy e1 e2

{- (\ v1 -> (\ v2 -> v1 (v2 v2)) (\ v2 -> v1 (v2 v2))) -}
{- |
>>> yai
ELambdaVars [1] (EBinary ApplyLazy (ELambdaVars [2] (EBinary ApplyLazy (EVar 1) (EBinary ApplyLazy (EVar 2) (EVar 2)))) (ELambdaVars [2] (EBinary ApplyLazy (EVar 1) (EBinary ApplyLazy (EVar 2) (EVar 2)))))
 -}
yai :: Expr' a
yai = ELambdaVars [1] (appExpr c1 c1)
  where c1 = ELambdaVars [2] $ appExpr (EVar 1) (appExpr (EVar 2) (EVar 2))

{- |
>>> testp rFix "fix (\\f n -> _ap f n)"
Right (EBinary ApplyLazy (ELambdaVars [1] (EBinary ApplyLazy (ELambdaVars [2] (EBinary ApplyLazy (EVar 1) (EBinary ApplyLazy (EVar 2) (EVar 2)))) (ELambdaVars [2] (EBinary ApplyLazy (EVar 1) (EBinary ApplyLazy (EVar 2) (EVar 2)))))) (ELambdaVars [1,2] (EBinary ApplyLazy (EVar 1) (EVar 2))))
>>> testp rUnary "_neg (_add 2 3)"
Right (EUnary Neg (EBinary Add (EInt 2) (EInt 3)))
 -}
rFix = string "fix" *> (appExpr yai <$> rAtom)
rUnary = EUnary <$> rUOp <*> rExpr

{- |
>>> testp rUOp "_neg"
Right Neg
 -}
rUOp :: Parser UOp
rUOp = asum [ rNeg, rNot, rS2I, rI2S ]
rNeg, rNot, rS2I, rI2S :: Parser UOp
rNeg = Neg <$ string "_neg"
rNot = Not <$ string "_not"
rS2I = StrToInt <$ string "_s2i"
rI2S = IntToStr <$ string "_i2s"

rBinary = EBinary <$> rBinOp <*> rAtom <*> rAtom

rBinOp :: Parser BinOp
rBinOp = asum
    [ rAdd
    , rSub
    , rMult
    , rDiv
    , rMod
    , rLt
    , rGt
    , rEql
    , rOr
    , rAnd
    , rConcat
    , rTake
    , rDrop
    , rApply
    , rApplyLazy
    , rApplyEager
    ]

rAdd, rSub, rMult, rDiv, rMod, rLt, rGt, rEql, rOr, rAnd,
  rConcat, rTake, rDrop, rApply, rApplyLazy, rApplyEager :: Parser BinOp
rAdd = Add <$ string "_add"
rSub = Sub <$ string "_sub"
rMult = Mult <$ string "_mult"
rDiv = Div <$ string "_qot"
rMod = Mod <$ string "_rem"
rLt = Lt <$ string "_lt"
rGt = Gt <$ string "_gt"
rEql = Eql <$ string "_eq"
rOr = Or <$ string "_or"
rAnd = And <$ string "_and"
rConcat = Concat <$ string "_cat"
rTake = Take <$ string "_tak"
rDrop = Drop <$ string "_dro"
rApply = Apply <$ string "_apn"
rApplyLazy = ApplyLazy <$ (string "_apz" <|> string "_ap")
rApplyEager = ApplyEager <$ string "_apv"

rIf =  string "_if" *> (EIf <$> rAtom <*> rAtom <*> rAtom)

hvchars, vchars :: String
hvchars = ['a'..'z']
vchars = hvchars ++ ['_'] ++ ['0'..'9'] ++ ['A'..'Z']

rVarName :: Parser String
rVarName = spaces *> ( (:) <$> satisfy (`elem` hvchars) <*> many (satisfy (`elem` vchars )) )

rArg :: Parser Var
rArg = do
  name <- rVarName
  cx@Cxt{varMap,nextVar} <- get
  let newVar = do
        let newMap = Map.insert name nextVar varMap
        put cx{varMap = newMap, nextVar = succ nextVar} $> nextVar
  maybe newVar pure $ Map.lookup name varMap

-- rLambda = undefined
{- |
 -}
rLambdaVars = ELambdaVars <$> ((char '\\') *> some rArg <* string "->") <*> rExpr

rVar = do
  name <- rVarName
  Cxt{varMap} <- get
  maybe (raise $ "var not defined: " ++ name) (pure . EVar) $ Map.lookup name varMap
