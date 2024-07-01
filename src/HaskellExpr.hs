{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}

module HaskellExpr where

import qualified Data.ByteString.Char8 as B8
import Data.Map (Map)
import qualified Data.Map as Map

import Imports hiding (And)
import Expr
import ParserLib hiding (Parser, raise, satisfy, token, eof, runParser)
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
    []         -> raise_ $ "HaskICFP.readable: " ++ tag ++ ": parse error: '" ++ take 5 input ++ "..."

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

rExpr :: Parser Expr
rExpr = asum
    [ rUnary
    , rBinary
    , rIf
    {- , rLambda -}
    , rLambdaVars
    , rAtom
    ]

rAtom :: Parser Expr
rAtom = char '(' *> rExpr <* char ')'
    <|> asum [rBool, rInt, rStr, rVar]

rBool ,rInt ,rStr ,rUnary ,rBinary ,rIf, {- rLambda, -}
  rLambdaVars, rVar :: Parser Expr

rBool = EBool <$> readable @Bool "EBool"
rInt  = EInt  <$> readable @Integer "EInt"
rStr  = EStr . B8.pack <$> readable @String "EStr"
rUnary = EUnary <$> rUOp <*> rExpr

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
{-
>>>
 -}
rLambdaVars = ELambdaVars <$> ((char '\\') *> some rArg <* string "->") <*> rExpr

rVar = do
  name <- rVarName
  Cxt{varMap} <- get
  maybe (raise $ "var not defined: " ++ name) (pure . EVar) $ Map.lookup name varMap
