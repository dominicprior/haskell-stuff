{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Data.List (intercalate)

type P st t = Parsec String st t

main = do
  putStrLn "hello world"


perlTokens :: P st [String]
perlTokens = many perlToken

perlToken :: P st String
perlToken =
  many1 space <|>
  comment <|>
  perlStr <|>
  dollarBraceSemicolon <|>
  try perlIf <|>
  try perlSub <|>
  try perlMy <|>
  perlVar <|>
  perlOp <|>
  perlBracket

comment :: P st String
comment = do
  char '#'
  str <- many $ noneOf "\n"
  char '\n'
  return $ '#' : str ++ "\n"

perlStr :: P st String
perlStr = do
  char '"'
  str <- concatMany perlStrChar
  char '"'
  return $ '"' : str ++ "\""

perlStrChar :: P st String
perlStrChar =
  (twoChars <$> char '\\' <*> anyChar) <|> (:[]) <$> noneOf "\""

twoChars :: Char -> Char -> String
twoChars a b = [a, b]

dollarBraceSemicolon :: P st String
dollarBraceSemicolon = oneOf "${};" >> return ""

perlIf :: P st String
perlIf = do
  string "if" >> spaces >> char '('
  toks <- perlTokens
  char ')' >> spaces >> char '{'
  return $ "if " ++ concat toks ++ ":"

perlDollarVar :: P st String
perlDollarVar = char '$' >> perlVar

perlMy :: P st String
perlMy = string "my" >> spaces >> return ""

perlVar :: P st String
perlVar = many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

perlOp :: P st String
perlOp = many1 $ oneOf "%^&*-+=.|<>~/"

perlBracket :: P st String
perlBracket = (:[]) <$> oneOf "[]"

perlSub :: P st String
perlSub = do
  string "sub" >> spaces
  name <- perlVar
  spaces >> char '{' >> spaces
  string "my" >> spaces >> char '('
  args <- perlDollarVar `sepBy1` (char ',' >> spaces)
  char ')' >> spaces >> char '=' >> spaces >> string "@_;\n"
  return $ "def " ++ name ++ "(" ++ intercalate ", " args ++ "):\n"

concatMany :: P st [a] -> P st [a]
concatMany = fmap concat . many

(<++>) :: P st [a] -> P st [a] -> P st [a]
p <++> q = (++) <$> p <*> q
infixl 5 <++>

stmt :: P st String
stmt = tern <++> (concatMany $ ass <++> tern)

tern :: P st String
tern = do
  e <- expr
  option e $ do
    th  <- char '?' >> tern
    els <- char ':' >> tern
    return $ th ++ " if " ++ e ++ " else " ++ els

expr :: P st String
expr = concatMany perlTok

perlTok :: P st String
perlTok =
  string "(" <++> stmt <++> string ")" <|>
  many1 space <|>
  comment <|>
  perlStr <|>
  perlVar


ass :: P st String
ass = (:[]) <$> oneOf "=,"

t p = parse p ""
fromRight (Right x) = x
tt = do
  str <- readFile "p.txt"
  putStr $ concat $ fromRight $ t perlTokens str

ss = do
  str <- readFile "p.txt"
  print $ t perlTokens str




