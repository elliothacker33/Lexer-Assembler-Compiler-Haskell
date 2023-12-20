import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P
import Text.Parsec.Expr
import qualified Data.Functor.Identity
import Data.Functor.Classes (eq1)

lexer =
  P.makeTokenParser emptyDef{
    P.reservedOpNames = ["+", "-", "*", "/",":=",";"]
  }

reservedOp = P.reservedOp lexer
parens     = P.parens lexer
natural    = P.natural lexer

identifier = P.identifier lexer

expr = buildExpressionParser table term

term = parens expr
    <|> numIE <$> natural
    <|> Var <$> identifier

parse_expr = expr

data Expr = Num Integer
                | Var String
                | Add Expr Expr
                | Sub Expr Expr
                | Mult Expr Expr
                | Div Expr Expr
                | Parens Expr
                | Negate Expr
                | Store String Expr
                  deriving (Eq,Show)

numE :: Integer -> Expr
numE = Num

numIE :: Integer -> Expr
numIE x = Num $ fromInteger x

multE :: (Expr->Expr->Expr)
multE = Mult

addE :: (Expr->Expr->Expr)
addE = Add

subE :: (Expr->Expr->Expr)
subE = Sub

divE :: (Expr->Expr->Expr)
divE =
  Div
newNegate :: (Expr -> Expr)
newNegate = Negate

store :: (Expr -> Expr -> Expr)
store (Var s) e1 = Store s e1

table = [ [prefix "-" newNegate,binary ":=" store AssocLeft],
  [binary "*" multE AssocLeft, binary "/" divE AssocLeft]
  , [binary "+" addE AssocLeft, binary "-" subE AssocLeft]
  ]

prefix :: String -> (a -> a) -> Operator String u Data.Functor.Identity.Identity a
prefix name f       = Prefix $ do {reservedOp name; return f}
postfix name f      = Postfix $ do {reservedOp name; return f}
binary name f assoc = Infix (do {reservedOp name; return f}) assoc

p1 :: String -> IO()
p1 = print . (parse parse_expr "input")

inputs = [
  "1"       , "1"     ,
  "-1"      , "-1"    ,
  "1"     , "1"   ,
  "2+1*2"     , "1/2"     ,
  "1+2"     , "1-2"     ,
  "(1+2)*2" , "(1-2)/2", "ok:=5;ok:=ok+1"
  ]

main = do
  mapM_ p1 inputs


