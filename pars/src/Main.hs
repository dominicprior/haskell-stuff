{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Data.List (intercalate)

type P st t = Parsec String st t

main = do
  putStrLn "hello world"

perlProg :: P st String
perlProg = concatMany stmt

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

perlIf :: P st String
perlIf = do
  e <- try (string "if") >> spaces >> char '(' >> expr <* char ')'
  return $ "if " ++ e ++ ":"

perlDollarVar :: P st String
perlDollarVar = char '$' >> perlVar

perlMy :: P st String
perlMy = try (string "my") >> spaces >> return ""

perlVar :: P st String
perlVar = many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

perlOp :: P st String
perlOp = many1 $ oneOf "%^&*-+=.|<>~/"

perlBracket :: P st String
perlBracket = (:[]) <$> oneOf "[]"

perlSub :: P st String
perlSub = do
  name <- try (string "sub") >> spaces >> perlVar
  spaces >> char '{' >> spaces
  string "my" >> spaces >> char '('
  args <- perlDollarVar `sepBy1` (char ',' >> spaces)
  char ')' >> spaces >> char '=' >> spaces >> string "@_;\n"
  return $ "def " ++ name ++ "(" ++ intercalate ", " args ++ "):\n"

concatMany :: P st [a] -> P st [a]
concatMany = fmap concat . many

concatMany1 :: P st [a] -> P st [a]
concatMany1 = fmap concat . many1

(<++>) :: P st [a] -> P st [a] -> P st [a]
p <++> q = (++) <$> p <*> q
infixl 5 <++>

stmt :: P st String
stmt =
  perlSub <|>
  perlIf <|>
  tern <++> (concatMany $ ass <++> tern) <|>
  (oneOf ";{}" >> return "")

tern :: P st String
tern = do
  e <- expr
  option e $ do
    th  <- char '?' >> tern
    els <- char ':' >> tern
    return $ th ++ " if " ++ e ++ " else " ++ els

expr :: P st String
expr = concatMany1 perlTok

perlTok :: P st String
perlTok =
  string "(" <++> stmt <++> string ")" <|>
  perlMy <|>
  many1 space <|>
  comment <|>
  perlStr <|>
  perlOp <|>
  (char '$' >> return "") <|>
  try perlVarNoSub

perlVarNoSub :: P st String
perlVarNoSub = do
  v <- perlVar
  if (v == "sub")
  then parserZero
  else return v


ass :: P st String
ass = (:[]) <$> oneOf "=,"

t p = parse p ""
fromRight (Right x) = x
tt = do
  str <- readFile "p.txt"
  putStr $ fromRight $ t perlProg str

ss = do
  str <- readFile "p.txt"
  print $ t perlProg str




