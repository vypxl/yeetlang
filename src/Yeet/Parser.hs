module Yeet.Parser (parseYeet) where

import Data.List
import Data.Void (Void)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L

--                                           yeet [prms] yeet callable [args] YEET
data Expr = Ident Integer | Number Integer | Fun  [Expr]      Expr     [Expr]

instance Show Expr where
  show (Ident x) = if x < 27 then ["_fxyzwabcdeghijklmnopqrstuv" !! (fromInteger x)] else 'x' : show x
  show (Number x) = show x
  show (Fun prms f args) = let
    paramsS = intercalate " " (map show prms)
    argsS = if (length args > 0) then intercalate " " (map show args ++ [""]) else ""
    in if (length prms) > 0 then  concat [ "( λ ", paramsS, " . ", show f, " ", argsS, ")" ]
    else concat [ "( ", show f, " ", argsS, ")" ]

type Parser = Parsec Void String

parseNumber :: String -> Expr
parseNumber = Number . go 0 . reverse
  where
    go _ [] = 0
    go i ('e':xs) = go (i+1) xs
    go i ('E':xs) = 2^i + go (i+1) xs

skipComment :: Parser ()
skipComment = L.space empty empty comment
  where
    skipSomeTill' p end = do
      r <- optional end
      case r of
        Nothing -> p >> skipManyTill p end
        Just _ -> empty
    yeet    = lookAhead . try . void $ between (string "yee") (char 't') (many (char 'e'))
    numb    = lookAhead . try . void $ (char 'Y') >> (char 'e' <|> char 'E') >> (some (char 'e' <|> char 'E')) >> (char 't')
    yEET    = lookAhead . try . void $ string "YEET"
    comment = skipSomeTill' anySingle (yeet <|> yEET <|> numb <|> (lookAhead eof))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipComment

ident :: Parser Expr
ident = Ident . fromIntegral . length <$> (label "identifier" . lexeme $ between (string "yee") (char 't') (some (char 'e')))

num :: Parser Expr
num = parseNumber <$> (label "number" . lexeme $ (:) <$> ((char 'Y') *> eE) <*> ((some eE) <* (char 't')))
  where
    eE = char 'e' <|> char 'E'

fun :: Parser Expr
fun = (uncurry3 Fun) <$> (label "function" . lexeme $ (,,) <$> (yeet *> mIdent) <*> ex <*> mEx)
  where
    uncurry3 f (a, b, c) = f a b c

    yeet           = lexeme $ string "yeet"
    yEET           = lexeme $ string "YEET"
    yeetBody       = label "yeet to enter function body" yeet
    yeetEnd        = label "closing yeet" yEET

    mIdent         = manyTill (label "parameter" ident) yeetBody

    ex             = label "argument" . choice . map try $ [fun, num, ident]
    mEx            = many ex <* yeetEnd

yeetParser :: Parser Expr
yeetParser = between skipComment eof fun

-----------------------
-- Exported Functions--
-----------------------

parseYeet :: String -> Either String Expr
parseYeet input = case out of
          Left err -> Left $ errorBundlePretty err
          Right f -> Right f
        where
          out = parse yeetParser "" input
