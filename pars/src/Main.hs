{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Data.List (intercalate)

type P = Parsec String () String
type PP a = Parsec String () a

main = do
  putStrLn "hello world"

perlProg :: P
perlProg = concatMany stmt

comment :: P
comment = do
  char '#'
  str <- many $ noneOf "\n"
  char '\n'
  return $ '#' : str ++ "\n"

normalStr :: Char -> P
normalStr c = enc c <$> (char c >> strInnards c)

strInnards :: Char -> P
strInnards c = concatMany (perlStrChar c) <* char c

perlStrChar :: Char -> P
perlStrChar c =
  (twoChars <$> char '\\' <*> anyChar) <|>
  (:[]) <$> noneOf [c]

twoChars :: Char -> Char -> String
twoChars a b = [a, b]

perlIf :: String -> String -> P
perlIf a b = do
  e <- tryStr a >> spaces >> char '(' >> expr <* char ')'
  return $ b ++ " " ++ e ++ ":"

perlPrint :: String -> P
perlPrint str = do
  e <- tryStr str >> spaces >> expr
  return $ str ++ "(" ++ e ++ ")"

perlPush :: P
perlPush = do
  a <- tryStr "push" >> spaces >> perlDollarVar <* char ',' <* spaces
  x <- tern
  return $ a ++ ".append(" ++ x ++ ")"

perlSplit :: P
perlSplit = do
  r <- tryStr "split" >> spaces >> char '/' >> strInnards '/' <* char ',' <* spaces
  x <- tern
  return $ "re.split(" ++ x ++ ", \"" ++ r ++ "\")"

perlFor :: P
perlFor = do
  x <- tryStr "for" >> spaces >> string "my" >> spaces >> perlDollarVar
  a <- spaces >> char '(' >> stmt <* char ')'
  return $ "for " ++ x ++ " in " ++ a ++ ":"

perlEnv :: P
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

perlDollarVar :: P
perlDollarVar = many1 (oneOf "@$") >> perlVar

perlMy :: P
perlMy = tryStr "my" >> spaces >> return ""

perlVar :: P
perlVar = many1 idChar

normalOps = ["&&", "||", "//", "<<", ">>", "+","-", "*", "/", "%", "&", "|", "^"]
assOps = map (++ "=") normalOps

matchAny :: [String] -> P
matchAny = foldr1 (<|>) . map tryStr

perlOpNoAss' :: P
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

perlOpNoAss :: P
perlOpNoAss = do
  op <- perlOpNoAss'
  if elem op assOps || op == ".="
  then parserZero
  else return op

ass :: P
ass =
  matchAny assOps <|>
  (tryStr ".=" >> return "+=") <|>
  strOneOf "=,"    

perlBracket :: P
perlBracket = strOneOf "[]"

perlSub :: P
perlSub = do
  name <- tryStr "sub" >> spaces >> perlVar
  (char ';' >> return "") <|> do
    spaces >> char '{'
    try (spaces >> perlSubWithArgs name) <|> (return $ "def " ++ name ++ "():\n")

perlSubWithArgs :: String -> P
perlSubWithArgs name = do
  string "my" >> spaces >> char '('
  args <- perlDollarVar `sepBy1` (char ',' >> spaces)
  char ')' >> spaces >> char '=' >> spaces >> string "@_;\n"
  return $ "def " ++ name ++ "(" ++ intercalate ", " args ++ "):\n"

concatMany :: PP [a] -> PP [a]
concatMany = fmap concat . many

concatMany1 :: PP [a] -> PP [a]
concatMany1 = fmap concat . many1

tryStr :: String -> P
tryStr = try . string

strOneOf :: String -> P
strOneOf = fmap (:[]) . oneOf

(<++>) :: PP [a] -> PP [a] -> PP [a]
p <++> q = (++) <$> p <*> q
infixl 5 <++>

stmt :: P
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

tern :: P
tern = do
  e <- expr
  option e $ do
    th  <- char '?' >> tern
    els <- char ':' >> tern
    return $ th ++ " if " ++ e ++ " else " ++ els

expr :: P
expr = concatMany1 perlTok

enc :: Char -> String -> String
enc c s = c : s ++ [c]

enc2 :: String -> String -> String -> String
enc2 beg end str = beg ++ str ++ end

perlTok :: P
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

perlqw :: P
perlqw = do
  tryStr "qw" >> spaces >> char '('
  a <- perlVar `sepEndBy` (many1 space) <* char ')'
  return $ "[" ++ (intercalate ", " $ map (enc '\'') a) ++ "]"

perlqq :: P
perlqq = do
  tryStr "qq" >> spaces >> char '('
  strInnards ')' >>= return . enc2 "qq(\"" "\")"


idChar :: PP Char
idChar = letter <|> digit <|> char '_'

regexpMatch :: String -> P
regexpMatch str =
  (char '/' >> regexpMatch' str '/') <|>
  ((char 'm' >> anyChar) >>= regexpMatch' str) <|>
  ((char 's' >> anyChar) >>= regexpSubst)

regexpMatch' :: String -> Char -> P
regexpMatch' str c = do
  s <- strInnards c
  return $ "." ++ str ++ "(" ++ enc '"' s ++ ")"

regexpSubst :: Char -> P
regexpSubst '(' = regexpSubst' ')'
regexpSubst '[' = regexpSubst' ']'
regexpSubst '{' = regexpSubst' '}'
regexpSubst c = do
  s1 <- strInnards c
  s2 <- strInnards c
  return $ ".sub(" ++ enc '"' s1 ++ ", " ++ enc '"' s2 ++ ")"

regexpSubst' :: Char -> P
regexpSubst' end = do
  s1 <- strInnards end
  beg2 <- spaces >> anyChar
  s2 <- strInnards $ oppositeBracket beg2
  return $ ".sub(" ++ enc '"' s1 ++ ", " ++ enc '"' s2 ++ ")"

oppositeBracket '(' = ')'
oppositeBracket '[' = ']'
oppositeBracket '{' = '}'

perlVarNoSub :: P
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




