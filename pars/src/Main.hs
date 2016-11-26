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
  perlParen <|>
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
  str <- many perlStrChar
  char '"'
  return $ '"' : concat str ++ "\""

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

perlSub :: P st String
perlSub = do
  string "sub" >> spaces
  name <- perlVar
  spaces >> char '{' >> spaces
  string "my" >> spaces >> char '('
  args <- perlDollarVar `sepBy1` (char ',' >> spaces)
  char ')' >> spaces >> char '=' >> spaces >> string "@_;\n"
  return $ "def " ++ name ++ "(" ++ intercalate ", " args ++ "):\n"

perlDollarVar :: P st String
perlDollarVar = char '$' >> perlVar

perlMy :: P st String
perlMy = string "my" >> spaces >> return ""

perlParen :: P st String
perlParen = do
  char '('
  toks <- perlTokens
  char ')'
  return $ "(" ++ concat toks ++ ")"

perlVar :: P st String
perlVar = many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

perlOp :: P st String
perlOp = many1 $ oneOf "%^&*-+=.|<>~/"

--perlExpr :: P st String
--perlExpr = 

perlBracket :: P st String
perlBracket = (:[]) <$> oneOf "[]"

t p = parse p ""
fromRight (Right x) = x
tt = do
  str <- readFile "p.txt"
  putStr $ concat $ fromRight $ t perlTokens str

ss = do
  str <- readFile "p.txt"
  print $ t perlTokens str




