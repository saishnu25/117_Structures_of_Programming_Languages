-- parser.hs
-- Exports a parsing function that takes a read file
--  as input and parse it into the full syntax, producing either a syntax
--  tree or a list of error-message lines as output
-- Imports: full.hs

module Parser where

import Full
import Control.Applicative hiding (many, some)
import Control.Monad
import Control.Monad.State.Strict
import Data.Text (Text)
import Data.Void
import Data.Char (isSpace)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Debug.Trace 
import System.IO
import Text.Megaparsec.Debug

type Parser = Parsec Void String

-- Using state so we can store a string that determines which paradigm
-- we are using for parsing based on user input.
type ParserT a = ParsecT Void String (State String) a




-- statements choice to determine which statement parsers are being included
stmtChoice :: String -> [ParserT FStmt]
stmtChoice c = case c of
  "functional" -> funcStmts
  "declarative" -> declStmts
  "declarative threaded" -> declThreadStmts
  "stateful" -> stateStmts
  "stateful threaded" -> stateThreadStmts
  where
    funcStmts = [pSkip, pBind, pLocal, pIf, pCase, pFunDef]
    declStmts = funcStmts ++ [pProcApp, pProcDef]
    declThreadStmts = declStmts ++ [pThread, pByNeed]
    stateStmts = declStmts ++ [pExchange, pNewCell, pSetCell]
    stateThreadStmts = stateStmts ++ [pThread, pByNeed]

-- Parseing function called in Hoz, consumes opening space and reads until eof
pStatementF :: ParserT [FStmt]
pStatementF = do
  between sc' eof pStatement


-- Parse statements based on the desired paradigm.
-- Sequence of statements is represented implicitly by returning a list of
-- statements.
pStatement :: ParserT [FStmt]
pStatement = do
  c <- get -- get retrieves state from monad representing input string by user
  let stmts = stmtChoice c
  s <- choice stmts <?> "Statement" -- <?> for error messages
  -- many is 0 or more
  ss <- (many . choice) stmts <?> "Statement" -- <?> for error messages
  return (s:ss)

pSkip :: ParserT FStmt
pSkip = do 
  rwordSC "skip"
  -- Fail after this point
  choice [pSkipB, pSkipS, pSkipF, pSkipC, pSkipSt, pSkipBr]
  where
    pSkipB = do (FSkip FBasic) <$ string "Basic" <* sc'
    pSkipS = do (FSkip FStore) <$ string "Store" <* sc'
    pSkipF = do (FSkip FFull) <$ string "Full" <* sc'
    pSkipC = do (FSkip FCheck) <$ string "Check" <* sc'
    pSkipSt = do (FSkip FStack) <$ string "Stack" <* sc'
    pSkipBr = do
      string "Browse" <* sc'
      FSkip <$> FBrowse <$> pIdentifierSC

pLocal :: ParserT FStmt
pLocal = do
  rwordSC "local"
  -- fail after this point
  inStmt <- pInStatement
  rwordSC "end"
  return $ FLocal inStmt -- the variable introduction is stored in inStmt

pBind :: ParserT FStmt
pBind = do
  id1 <- try $ do pPattern <* charSC '=' -- make atomic using do/try
  -- fail after this point
  ex <- pExp
  return $ FEq id1 (Ex ex)

pIf :: ParserT FStmt
pIf = do
  rwordSC "if"
  -- Fail after this point
  exp1 <- pExp
  rwordSC "then"
  inStm <- pInStatement
  nestedElseIf <- many pElseIF
  optElseCheck <- optional (rwordSC "else") -- optional else statement
  optElse <- case optElseCheck of
    (Just s) -> do Just <$> pInStatement
    Nothing  -> do Nothing <$ sc'
  rwordSC "end"
  return $ FIf exp1 inStm nestedElseIf optElse
  where
    pElseIF = do 
      rwordSC "elseif"
      exp1 <- pExp
      rwordSC "then"
      inStm <- pInStatement
      return (exp1, inStm)

pCase :: ParserT FStmt
pCase = do
  rwordSC "case"
  -- Fail after this point
  id1 <- pIdentifierSC
  rwordSC "of"
  patt <- pPattern -- Cannot be Variable! This is checked in elab
  rwordSC "then"
  inStmt <- pInStatement
  nestedCase <- many pNestCase
  optElseCheck <- optional (rwordSC "else")
  optElse <- case optElseCheck of
    Just s -> do Just <$> pInStatement
    Nothing  -> do Nothing <$ sc'
  rwordSC "end"
  return $ FCase id1 patt inStmt nestedCase optElse
  where 
    pNestCase = do
      string "[]" <* sc'
      patt <- pPattern
      rwordSC "then"
      inStmt <- pInStatement
      return (patt, inStmt)

pProcApp :: ParserT FStmt
pProcApp = do 
  charSC '{'
  -- Fail after this point
  procID <- pIdentifierSC
  params <- many pExp
  charSC '}'
  return $ FApply procID params

-- read a proc definition and convert it to the FEq type
pProcDef :: ParserT FStmt
pProcDef = do
  rwordSC "proc"
  -- Fail after this point
  charSC '{'
  procID <- pIdentifierSC
  params <- many pIdentifierSC
  charSC '}'
  body <- pInStatement
  rwordSC "end"
  return $ FEq (PtVar procID) $ Pr params body

-- read a func definition and convert it to the FEq type
pFunDef :: ParserT FStmt
pFunDef = do
  rwordSC "fun"
  -- Fail after this point
  charSC '{'
  funID <- pIdentifierSC
  params <- many pIdentifierSC
  charSC '}'
  body <- pInExpression
  rwordSC "end"
  return $ FEq (PtVar funID) $ Fn params body

pThread :: ParserT FStmt
pThread = do
  rwordSC "thread"
  -- Fail after this point
  inStmt <- pInStatement
  rwordSC "end"
  return $ FThread inStmt

pByNeed :: ParserT FStmt
pByNeed = do
  rwordSC "byNeed"
  -- Fail after this point
  ex <- pExp
  id1 <- pIdentifierSC
  return $ FByNeed ex id1

pNewCell :: ParserT FStmt
pNewCell = do
  rwordSC "newCell"
  -- Fail after this point
  ex  <- pExp
  id1 <- pIdentifierSC
  return $ FNewCell ex id1

pExchange :: ParserT FStmt
pExchange = do
  rwordSC "exchange"
  -- Fail after this point
  id1 <- pIdentifierSC
  id2 <- pIdentifierSC
  ex  <- pExp
  return $ FExchange id1 id2 ex

pSetCell :: ParserT FStmt
pSetCell = do
  id1 <- try $ pIdentifierSC <* stringSC ":=" -- make atomic using try
  ex <- pExp
  return $ FSetCell id1 ex

-- TRY CATCH AND RAISE STATEMNETS 


-- InStatements
pInStatement :: ParserT InStmt
pInStatement = do 
  decPart <- (optional . try) pDecPart -- optional necessary so it does not get
                                       -- confused with an Eq statement
  stmts <- pStatement
  return (decPart, stmts)

data DecChoice = Bnd (Patt, Exp) | Vr FVar -- List of lone varaibles or binds
pDecPart :: ParserT DecPart
pDecPart = do
  l <- many $ (pBnd <|> pVr) -- pBnd first because it uses try to get '='
  rwordSC "in"
  let (vars,binds) = splitDecChoice l
  return (vars,binds)
  where
    pBnd = do 
      patt <- try $ do pPattern <* charSC '=' -- make atomic using do/try
      ex <- pExp
      return $ Bnd (patt, ex)
      --Bnd <$> ((,) <$> (pPattern <* charSC '=') <*> pExp)
    pVr = do Vr <$> pIdentifierSC
    -- lone variables come first, then binds
    splitDecChoice [] = ([],[])
    splitDecChoice ((Bnd b):l) = (as,b:bs)
      where (as,bs) = splitDecChoice l
    splitDecChoice ((Vr v):l) = (v:as,bs)
      where (as,bs) = splitDecChoice l

-- Expressions

pExStatements :: ParserT ([FStmt], Exp)
pExStatements = do 
  c <- get
  let stmts = stmtChoice c
  s <- (optional . try) $ do choice stmts -- Are you at final expression?
  (stmts,exp) <- case s of
    Just (FApply id params) -> (do
      optS <- (optional . try) pExStatements
      let 
        (stmts1,exp1) = case optS of
          Just (ss,ex) -> ((FApply id params):ss,ex)
          Nothing -> ([], (EFunApp id params))
      return (stmts1, exp1))
    Just s -> do
      (ss,ex) <- pExStatements
      return (s:ss,ex)
    Nothing -> do 
      e <- pExp
      return ([],e)
  return (stmts, exp)

pInExpression :: ParserT InExp
pInExpression = do
  decPart <- (optional . try) pDecPart
  (stmts,ex) <- pExStatements
  return (decPart, stmts, ex)


rbos :: [Char]
rbos = ['+','-','*','/','<','>']

opPairs :: [(String,String)]
opPairs = [("+","IntPlus"),("-","IntMinus"),("*","IntMultiply"),("/","Divide"),("<","LT"),(">","GT"),
  ("div","DivideInt"),("mod","Mod"),("==","Eq"),("\\=","NotEqual"),("=<","LessThanEq"),("=>","GreaterThanEq")]

rBinOp :: ParserT String
rBinOp = (:[]) <$> satisfy (\x-> elem x rbos)
  <|> string "div"
  <|> string "mod"
  <|> string "=="
  <|> string "=<"
  <|> string "\\="
  <|> string "=>"

-- remove Maybe constructors
optStmt :: [Maybe FStmt] -> [FStmt]
optStmt ((Just s):ss) = s:(optStmt ss)
optStmt (Nothing:ss) = []

pExp :: ParserT Exp
pExp = do 
  c <- get
  let exprs = exprChoice c
  choice exprs
  where
    -- Select parsers to use for the expression based on user input string
    funcExps = [pENum, pEVar, pParen, pExpRcd, pExpAtom, pExpList, pLocal, pIf, pCase, pFun, pFunApp]
    declExps = funcExps ++ [pProc]
    declThreadExps = declExps ++ [pThread]
    stateExps = declExps ++ [pNewCell, pAtCell]
    stateThreadExps = stateExps ++ [pThread]
    exprChoice :: String -> [ParserT Exp]
    exprChoice c = case c of
      "functional" -> funcExps
      "declarative" -> declExps
      "declarative threaded" -> declThreadExps
      "stateful" -> stateExps
      "stateful threaded" -> stateThreadExps
    pENum = do ENum <$> pNum <* sc'
    pEVar = do EVar <$> pIdentifierSC
    pExpList = do
      charSC '['
      elems <- many pExp
      charSC ']'
      return $ EList elems
    pParen = do
      charSC '('
      e1 <- pExp
      op <- (rBinOp <|> (:[]) <$> rcBinOp) <* sc' -- operation or # or |
      e2 <- pExp
      charSC ')'
      return $ EParen e1 e2 op
    pExpRcd = do
      label <- try $ do pAtom <* charSC '('
      es <- many $ (,) <$> pFeatureSC <* charSC ':' <*> pExp 
      charSC ')'
      return $ ERcd label es
    pExpAtom = do
      label <- pAtom <* sc'
      return $ ERcd label []
    pFun = do 
      rwordSC "fun"
      charSC '{'
      charSC '$' -- function as an expression
      params <- many pIdentifierSC
      charSC '}'
      inExp <- pInExpression
      rwordSC "end"
      return $ EFun params inExp
    pProc = do 
      rwordSC "proc"
      charSC '{'
      charSC '$' -- function as an expression
      params <- many pIdentifierSC
      charSC '}'
      inStmt <- pInStatement
      rwordSC "end"
      return $ EProc params inStmt
    pLocal = do
      rwordSC "local"
      -- fail after this point
      inExp <- pInExpression
      rwordSC "end"
      return $ ELocal inExp
    pIf = do
      rwordSC "if"
      -- Fail after this point
      exp1 <- pExp
      rwordSC "then"
      inExp <- pInExpression
      nestedElseIf <- many pElseIF
      optElseCheck <- optional (rwordSC "else") -- optional else statement
      optElse <- case optElseCheck of
        (Just s) -> do Just <$> pInExpression
        Nothing  -> do Nothing <$ sc'
      rwordSC "end"
      return $ EIf exp1 inExp nestedElseIf optElse
      where
        pElseIF = do 
          rwordSC "elseif"
          exp1 <- pExp
          rwordSC "then"
          inExp <- pInExpression
          return (exp1, inExp)
    pCase = do
      rwordSC "case"
      -- Fail after this point
      id1 <- pIdentifierSC
      rwordSC "of"
      patt <- pPattern 
      rwordSC "then"
      inExp <- pInExpression
      nestedCase <- many pNestCase
      optElseCheck <- optional (rwordSC "else")
      optElse <- case optElseCheck of
        (Just s) -> do Just <$> pInExpression
        Nothing  -> do Nothing <$ sc'
      rwordSC "end"
      return $ ECase id1 patt inExp nestedCase optElse
      where 
        pNestCase = do
          string "[]" <* sc'
          patt <- pPattern -- Update to be more versatile
          rwordSC "then"
          inExp <- pInExpression
          return (patt, inExp)
    pThread = do
      rwordSC "thread"
      -- Fail after this point
      inExp <- pInExpression
      rwordSC "end"
      return $ EThread inExp
    pNewCell = do
      rwordSC "newCell"
      -- Fail after this point
      ex  <- pExp
      return $ ENewCell ex
    pFunApp = do 
      charSC '{'
      funID <- pIdentifierSC
      params <- many pExp
      charSC '}'
      return $ EFunApp funID params
    pAtCell = do
      char '@'
      id1 <- pIdentifierSC
      return $ EAtCell id1



-- Patterns

rcbos :: [Char]
rcbos = ['#','|']

rcBinOp :: ParserT Char
rcBinOp = satisfy (\x-> elem x rcbos)

pPattern:: ParserT Patt
pPattern =
  (( PtVar <$> pIdentifierSC) -- Var included for inDeclaration, not used for case
    <|> pPattList
    <|> pPattRcd 
    <|> pParen
    <|> pPAtom) <?> "Pattern"
  where
    pPattList = do
      charSC '['
      elems <- many pPattern
      charSC ']'
      return $ PtList elems
    pParen = do
      charSC '('
      pt1 <- pPattern
      op <- rcBinOp <* sc'
      pt2 <- pPattern
      charSC ')'
      return $ PtParen pt1 pt2 op
    pPattRcd = do
      label <- try $ do pAtom <* charSC '('
      ps <- many $ (,) <$> pFeatureSC <* charSC ':' <*> pPattern 
      charSC ')'
      return $ PtRec label ps
    pPAtom = do 
      label <- pAtom <* sc'
      return $ PtRec label []


-- Basic helper functions for parsing: white space consumer and values

-- MarkKaprov tutorial (parsing a simple imperative language)
-- space consumer
sc :: ParserT ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- consume 0 or more whitespace, comments, newlines (all inclusive whitespace consumer)
-- not sure if this works correctly because skipLineComment and skipBlockComment
sc' :: ParserT ()
sc' = do
  s <- optional (some $ satisfy (isSpace))
  void (optional (L.skipLineComment "//"))
  void (optional (L.skipBlockComment ("/*") ("*/")))
  n <- optional (some $ satisfy (== '\n'))
  (if (mh s n) then return () else (() <$ sc'))
  where
    mh Nothing Nothing = True
    mh _ _ = False

-- wrap lexeme so it parses white space after it
lexeme :: ParserT a -> ParserT a
lexeme = L.lexeme sc

-- parse string and whitespace after
symbol :: String -> ParserT String
symbol = L.symbol sc

-- char wrapped with space consumer
charSC :: Char -> ParserT Char
charSC c = char c <* sc'

stringSC :: String -> ParserT String
stringSC s = string s <* sc'

-- reserved word parsers followed by at least one space
rword :: String -> ParserT String
rword w = (lexeme . try) (string w <* notFollowedBy alphaNumChar)

rwordSC :: String -> ParserT String
rwordSC w = rword w <* sc'

-- reserved words
rws :: [String]
rws = ["skip","local","in","end","case","of","if","else","then","proc","Browse","elseif","Garb","thread","byNeed","newCell", "fun"]

-- parsing an identifier 
-- starts with upper case letter, cannot be a reserved word
-- no underscore, no quote names
pIdentifier :: ParserT String
pIdentifier = try (p >>= check)
  where
    p = (:) <$> upperChar <*> many alphaNumChar
    check x = if x `elem` rws
          then fail $ "keyword " ++ show x ++ " cannot be an identifier"
          else return x

pIdentifierSC :: ParserT String
pIdentifierSC = pIdentifier <* sc <?> "Identifier" -- expected identifier

-- parsing an atom
-- starts with lower case letter, cannot be reserved word
-- OR
-- starts with single quote, ends with single quote
pAtom :: ParserT String 
pAtom = try ( (p >>= check) <|> pSingleQuotes) <?> "Atom"
  where
    p = (:) <$> lowerChar <*> many alphaNumChar
    check x = if x `elem` rws
          then fail $ "keyword " ++ show x ++ " cannot be an atom"
          else return x
    pSingleQuotes = do 
      x <- singleQuotes
      return ("\'"++x++"\'")
    singleQuotes = between (char '\'') (char '\'') (many (satisfy (not . (== '\''))))


-- https://mmhaskell.com/parsing-4 for number example

pNum :: ParserT FPNum
pNum = (FPFloat <$> pFloat 
  <|> FPInt <$> pInteger
  <|> pNegative)  <?> "Number"
  where
    pFloat :: ParserT Float
    pFloat = try $ do
      whole <- many digitChar <* (char '.')
      fractional <- many digitChar
      let num = read $ whole ++ ('.':fractional)
      return num
    pInteger :: ParserT Int
    pInteger = try $ do
      int <- some digitChar
      let num = read int
      return num
    pNegative :: ParserT FPNum
    pNegative = try $ do
      void (char '-')
      num <- many digitChar
      dec <- (optional . try) (char '.')
      case dec of 
        Just _ -> do
          fractional <- many digitChar
          let fnum = read $ num ++ ('.':fractional)
          return $ FPFloat (-1 * fnum)
        Nothing -> return $ FPInt (-1* (read num))
      

pLiteral :: ParserT String
pLiteral = pAtom <?> "Literal"

pFeatureSC :: ParserT String
pFeatureSC = do (pFeature <* sc') <?> "Feature"

pFeature :: ParserT String
pFeature = 
  try pAtom <|>
  do 
    n1 <- digitChar
    num <- many digitChar
    return (n1:num)


































