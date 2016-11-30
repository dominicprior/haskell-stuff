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

normalStr :: Char -> P st String
normalStr c = enc c <$> (char c >> strInnards c)

strInnards :: Char -> P st String
strInnards c = concatMany (perlStrChar c) <* char c

perlStrChar :: Char -> P st String
perlStrChar c =
  (twoChars <$> char '\\' <*> anyChar) <|>
  (:[]) <$> noneOf [c]

twoChars :: Char -> Char -> String
twoChars a b = [a, b]

perlIf :: String -> String -> P st String
perlIf a b = do
  e <- tryStr a >> spaces >> char '(' >> expr <* char ')'
  return $ b ++ " " ++ e ++ ":"

perlPrint :: String -> P st String
perlPrint str = do
  e <- tryStr str >> spaces >> expr
  return $ str ++ "(" ++ e ++ ")"

perlPush :: P st String
perlPush = do
  a <- tryStr "push" >> spaces >> perlDollarVar <* char ',' <* spaces
  x <- tern
  return $ a ++ ".append(" ++ x ++ ")"

perlSplit :: P st String
perlSplit = do
  r <- tryStr "split" >> spaces >> char '/' >> strInnards '/' <* char ',' <* spaces
  x <- tern
  return $ "re.split(" ++ x ++ ", \"" ++ r ++ "\")"

perlFor :: P st String
perlFor = do
  x <- tryStr "for" >> spaces >> string "my" >> spaces >> perlDollarVar
  a <- spaces >> char '(' >> stmt <* char ')'
  return $ "for " ++ x ++ " in " ++ a ++ ":"

perlEnv :: P st String
perlEnv =
  tryStr "ENV{" >> (
    (
      (many idChar <* char '}') >>= return . enc2 "os.environ['" "']"
    )
   <|>
    (
      (expr <* char '}') >>= return . enc2 "os.environ[" "]"
    )
  )

perlDollarVar :: P st String
perlDollarVar = many1 (oneOf "@$") >> perlVar

perlMy :: P st String
perlMy = tryStr "my" >> spaces >> return ""

perlVar :: P st String
perlVar = many1 idChar

normalOps = ["&&", "||", "//", "<<", ">>", "+","-", "*", "/", "%", "&", "|", "^"]
assOps = map (++ "=") normalOps

matchAny :: [String] -> P st String
matchAny = foldr1 (<|>) . map tryStr

perlOpNoAss' :: P st String
perlOpNoAss' =
  matchAny assOps <|> tryStr ".=" <|>
  matchAny ["<=", ">=", "==", "!=", "<<", ">>", "**"] <|>
  (tryStr "++" >> return " += 1") <|>
  (tryStr "&&" >> return "and") <|>
  (tryStr "||" >> return "or")  <|>
  (tryStr "=>" >> return ":")  <|>
  (tryStr "!"  >> return "not") <|>
  (tryStr "."  >> return "+")   <|>
  matchAny normalOps <|>
  matchAny ["[", "]", "<", ">"]

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
  (char ';' >> return "") <|> do
    spaces >> char '{'
    try (spaces >> perlSubWithArgs name) <|> (return $ "def " ++ name ++ "():\n")

perlSubWithArgs :: String -> P st String
perlSubWithArgs name = do
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
  perlIf "if" "if" <|>
  perlIf "elsif" "elif" <|>
  perlIf "while" "while" <|>
  perlFor <|>
  perlPrint "print" <|>
  perlPrint "system_or_die" <|>
  perlPrint "chdir_or_die" <|>
  perlPrint "die" <|>
  perlPush <|>
  perlSplit <|>
  (tryStr "else" >> return "else:") <|>
  tern <++> (concatMany $ ass <++> tern) <|>
  (oneOf ";{}" >> return "") <|>
  (tryStr "use" >> many (noneOf "\n") >> char '\n' >> return "")

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

enc2 :: String -> String -> String -> String
enc2 beg end str = beg ++ str ++ end

perlTok :: P st String
perlTok =
  tryStr "()" <|>
  string "(" <++> stmt <++> string ")" <|>
  perlMy <|>
  many1 space <|>
  perlqw <|>
  perlqq <|>
  perlEnv <|>
  comment <|>
  normalStr '"' <|>
  normalStr '\'' <|>
  (tryStr "!~" >> spaces >> regexpMatch "nomatch") <|>
  try perlOpNoAss <|>
  (oneOf "$@" >> return "") <|>
  try perlVarNoSub <|>
  (tryStr "=~" >> spaces >> regexpMatch "match")

perlqw :: P st String
perlqw = do
  tryStr "qw" >> spaces >> char '('
  a <- perlVar `sepEndBy` (many1 space) <* char ')'
  return $ "[" ++ (intercalate ", " $ map (enc '\'') a) ++ "]"

perlqq :: P st String
perlqq = do
  tryStr "qq" >> spaces >> char '('
  strInnards ')' >>= return . enc2 "qq(\"" "\")"


idChar :: P st Char
idChar = letter <|> digit <|> char '_'

regexpMatch :: String -> P st String
regexpMatch str =
  (char '/' >> regexpMatch' str '/') <|>
  ((char 'm' >> anyChar) >>= regexpMatch' str) <|>
  ((char 's' >> anyChar) >>= regexpSubst)

regexpMatch' :: String -> Char -> P st String
regexpMatch' str c = do
  s <- strInnards c
  return $ "." ++ str ++ "(" ++ enc '"' s ++ ")"

regexpSubst :: Char -> P st String
regexpSubst '(' = regexpSubst' ')'
regexpSubst '[' = regexpSubst' ']'
regexpSubst '{' = regexpSubst' '}'
regexpSubst c = do
  s1 <- strInnards c
  s2 <- strInnards c
  return $ ".sub(" ++ enc '"' s1 ++ ", " ++ enc '"' s2 ++ ")"

regexpSubst' :: Char -> P st String
regexpSubst' end = do
  s1 <- strInnards end
  beg2 <- spaces >> anyChar
  s2 <- strInnards $ oppositeBracket beg2
  return $ ".sub(" ++ enc '"' s1 ++ ", " ++ enc '"' s2 ++ ")"

oppositeBracket '(' = ')'
oppositeBracket '[' = ']'
oppositeBracket '{' = '}'

perlVarNoSub :: P st String
perlVarNoSub = do
  v <- perlVar
  if elem v $ printOps ++ ["sub", "if", "for", "while", "else", "elsif",
                           "push", "use", "die"]
  then parserZero
  else return $ varOp v

varOp :: String -> String
varOp "eq" = "=="
varOp "ne" = "!="
varOp "ge" = ">="
varOp "le" = "<="
varOp "gt" = ">"
varOp "lt" = "<"
varOp x = x

printOps = ["print", "system_or_die", "chdir_or_die"]

t p = parse p ""
fromRight (Right x) = x
tt = do
  str <- readFile "p.pl"
  putStr $ fromRight $ t perlProg str

ss = do
  str <- readFile "p.pl"
  print $ t perlProg str




