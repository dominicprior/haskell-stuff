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

normalStr :: P st String
normalStr = enc '"' <$> (char '"' >> strInnards c <* char '"')

strInnards :: Char -> P st String
strInnards c = char '"' >> concatMany $ perlStrChar c <* char '"'

perlStrChar :: Char -> P st String
perlStrChar c =
  (twoChars <$> char '\\' <*> anyChar) <|> (:[]) <$> noneOf c

twoChars :: Char -> Char -> String
twoChars a b = [a, b]

perlIf :: P st String
perlIf = do
  e <- tryStr "if" >> spaces >> char '(' >> expr <* char ')'
  return $ "if " ++ e ++ ":"

perlDollarVar :: P st String
perlDollarVar = char '$' >> perlVar

perlMy :: P st String
perlMy = tryStr "my" >> spaces >> return ""

perlVar :: P st String
perlVar = many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

normalOps = ["&&", "||", "//", "<<", ">>", "+","-", "*", "/", "%", "&", "|", "^"]
assOps = map (++ "=") normalOps

matchAny :: [String] -> P st String
matchAny = foldr1 (<|>) . map tryStr

perlOpNoAss' :: P st String
perlOpNoAss' =
  matchAny assOps <|> tryStr ".=" <|>
  matchAny ["<=", ">=", "==", "!=", "<<", ">>", "**"] <|>
  (tryStr "&&" >> return "and") <|>
  (tryStr "||" >> return "or")  <|>
  (tryStr "!"  >> return "not") <|>
  (tryStr "."  >> return "+")   <|>
  matchAny normalOps

perlOpNoAss :: P st String
perlOpNoAss = do
  op <- perlOpNoAss'
  if elem op assOps || op == ".="
  then parserZero
  else return op

ass :: P st String
ass =
  matchAny assOps <|>
  (tryStr ".=" >> return "+=") <|>
  strOneOf "=,"    


perlBracket :: P st String
perlBracket = strOneOf "[]"

perlSub :: P st String
perlSub = do
  name <- tryStr "sub" >> spaces >> perlVar
  spaces >> char '{' >> spaces
  string "my" >> spaces >> char '('
  args <- perlDollarVar `sepBy1` (char ',' >> spaces)
  char ')' >> spaces >> char '=' >> spaces >> string "@_;\n"
  return $ "def " ++ name ++ "(" ++ intercalate ", " args ++ "):\n"

concatMany :: P st [a] -> P st [a]
concatMany = fmap concat . many

concatMany1 :: P st [a] -> P st [a]
concatMany1 = fmap concat . many1

tryStr :: String -> P st String
tryStr = try . string

strOneOf :: String -> P st String
strOneOf = fmap (:[]) . oneOf

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

enc :: Char -> String -> String
enc c s = c : s ++ [c]

perlTok :: P st String
perlTok =
  string "(" <++> stmt <++> string ")" <|>
  perlMy <|>
  many1 space <|>
  comment <|>
  normalStr <|>
  try perlOpNoAss <|>
  (char '$' >> return "") <|>
  try perlVarNoSub <|>
  tryStr "=~" >> spaces >> regexpMatch

regexpMatch :: P st String
regexpMatch =
  --(char '/' >> regexpMatch' '/') <|>
  undefined
  --((char 'm' >> anyChar) >>= regexpMatch')

regexpMatch' :: Char -> P st String
regexpMatch' c = do
  s <- strInnards c <* char c
  return $ ".match(" ++ s ++ ")"

perlVarNoSub :: P st String
perlVarNoSub = do
  v <- perlVar
  if (v == "sub")
  then parserZero
  else return v


t p = parse p ""
fromRight (Right x) = x
tt = do
  str <- readFile "p.txt"
  putStr $ fromRight $ t perlProg str

ss = do
  str <- readFile "p.txt"
  print $ t perlProg str




