-- interp.hs
-- Exports a function that elaborates (converts) full to kernel. Should not fail 
--  (language problems should be detectable during parsing)
--  Failure if an Identifier is used instead of a record/pattern in case statement.
-- Imports: kernel.hs, full.hs

module Elab where

import Kernel
import Full


-- Input: list of statements from full, integer for creating new unique names
-- Output: list of statements in the kernel syntax

-- Naming. Integer cnt is passed recursively through function calls, being
-- incremented whenever a new name is created. Naming is done individually:
--    e.g., exID = "EXU" ++ show (cnt+1)
-- or in a list: 
--    e.g., exIDs = ["EXU" ++ show (cnt+i)| i<-[1..(length exps)]]
-- Expressions use EXU, NewCell use NCU, ByNeed BNU, Patterns use PTU, 
-- SetCell use SCU and GarbU (unused variable in exchange)
-- Expressions are renamed by default so " if X then ..."
-- becomes :
--    local EXU1 in 
--      EXU1 = X 
--      if EXU1 then ...
--  Special case for unecessary renaming of identifiers is not handled


-- Recursively move through statements, using pattern matching to distinguish
-- statement types. 
elab :: [FStmt] -> Int -> [Stmt]
elab [] cnt = [] 
elab (f:fs) cnt = elabFront ++ elab fs cnt
  where
    elabFront = case f of
      FSkip skipType -> [Skip (sTrans skipType)]
      FThread inStmt -> [Thread (inStmtH cnt inStmt)]
      FLocal inStmt -> inStmtH cnt inStmt -- Variable declarations contained
                                          -- inStmt (Local is redundant)
      FEq (PtVar x) (Ex ex) -> expH cnt ex x -- Variable case
      FEq patt (Ex ex) -> fullK
        where
          (p1,p2,p3,p4) = pattH cnt patt
          exStmts = expH cnt ex p4
          fullK = if p2 == [] then p3 ++ exStmts else [Local p2 (p3 ++ exStmts)]
      FEq (PtVar x) (Pr params body) -> fullK -- pattern = procedure
        where 
          bodyN = inStmtH cnt body 
          proc = PProc params bodyN
          fullK = [EqVal x proc]
      FEq (PtVar x) (Fn params body) -> fullK -- pattern = function
        where
          exID = "EXU" ++ show (cnt+1)
          bodyN = inExpH body (cnt+1) exID
          proc = PProc (params ++ [exID]) bodyN
          fullK = [EqVal x proc] 
      FIf ex inStmt [] mElse -> fullK -- no nested else if
        where
          exID = "EXU" ++ show (cnt+1)
          exStmts = expH (cnt + 1) ex exID
          inStmts = inStmtH cnt inStmt
          oElse = case mElse of 
            Nothing -> [Skip Basic] -- skip if no else
            Just (inStmt) -> inStmtH cnt inStmt
          ifStmt = If exID inStmts oElse
          fullK = [Local [exID] (exStmts ++ [ifStmt])]
      FIf ex inStmt ((ex2,inStmt2):elseIfs) mElse -> fullK 
        where
          exID = "EXU" ++ show (cnt+1)
          exStmts = expH (cnt+1) ex exID
          inStmts = inStmtH cnt inStmt
          -- rescurive call on nested else if statement 
          nestStmts = elab [(FIf ex2 inStmt2 elseIfs mElse)] (cnt+1)
          ifStmt = If exID inStmts nestStmts
          fullK = [Local [exID] (exStmts ++ [ifStmt])]
      FCase id1 patt inStmt [] mElse -> fullK -- no nested options
        where
          (p1,p2,p3,p4) = pattH cnt patt
          recLit = case (last p3) of -- make sure pattern is record and not variable
            (EqVal _ (PRec x)) -> x
            _ -> error "case called on non record pattern"
          oElse = case mElse of 
            Nothing -> [Skip Basic] -- skip if no else
            Just (inStmt) -> inStmtH cnt inStmt
          caseStmt = Case id1 recLit (inStmtH cnt inStmt) oElse
          fullK = if (init p2) == [] then init p3 ++ [caseStmt]
            else [Local (init p2) (init p3 ++ [caseStmt])]
      FCase id1 patt inStmt ((patt2,inStmt2):opts) mElse -> fullK
        where
          (p1,p2,p3,p4) = pattH cnt patt
          recLit = case (last p3) of -- make sure pattern is record and not variable
            (EqVal _ (PRec x)) -> x
            _ -> error "case called on non record pattern"
          -- rescurive call on nested option statement 
          nestStmts = elab [(FCase id1 patt2 inStmt2 opts mElse)] cnt
          caseStmt = Case id1 recLit (inStmtH cnt inStmt) nestStmts
          fullK = if (init p2) == [] then init p3 ++ [caseStmt]
            else [Local (init p2) (init p3 ++ [caseStmt])]
      FApply procID params -> fullK
        where
          -- creating expression IDs
          exIDs = ["EXU" ++ show (cnt+i)| i<-[1..(length params)]]
          -- Binding expression IDs to expression values
          exStmts = concat $ map (\(a,b) -> (expH (cnt+length params)) a b) $ zip params exIDs
          applyStmt = Apply procID exIDs
          fullK = if exIDs == [] then exStmts ++ [applyStmt]
            else [Local exIDs ( exStmts ++ [applyStmt])]
      FNewCell ex id1 -> fullK
        where
          id2 = "NCU" ++ show (cnt+1)
          exStmts = expH (cnt+1) ex id2
          fullK = [Local [id2] (exStmts ++ [NewCell id2 id1])]
      FExchange id1 id2 ex -> fullK
        where
          id3 = "NCU" ++ show (cnt+1)
          exStmts = expH (cnt+1) ex id3
          fullK = [Local [id3] (exStmts ++ [Exchange id1 id2 id3])]
      FSetCell id1 ex -> fullK
        where
          exID = "SCU" ++ show (cnt+1)
          id2 = "GarbU" ++ show (cnt+1)
          exStmts = expH (cnt+1) ex exID
          fullK = [Local [exID,id2] (exStmts ++ [Exchange id1 id2 exID])]
      FByNeed ex id1 -> fullK
        where
          id2 = "BNU" ++ show (cnt+1)
          exStmts = expH (cnt+1) ex id2
          fullK = [Local [id2] (exStmts ++ [ByNeed id2 id1])]
    -- Translate skips
    sTrans FBasic = Basic
    sTrans FStore = Store
    sTrans FFull = Full
    sTrans FCheck = Check
    sTrans (FBrowse s) = Browse s
    sTrans FStack = Stack 

-- Nesting caused by introducing variables with Local, so the statements are 
-- passed along to decPartH and bindH where they can be placed within the inner
-- most local
inStmtH :: Int -> InStmt -> [Stmt]
inStmtH cnt (Just decPart ,fstmts) = decPartH decPart cnt $ elab fstmts cnt
inStmtH cnt (Nothing, fstmts) = elab fstmts cnt

-- Handles the declarations of the in statement
decPartH :: DecPart -> Int -> [Stmt] -> [Stmt]
decPartH ([], binds) cnt ss = bindsH binds cnt ss
decPartH (ids, binds) cnt ss = [Local ids $ bindsH binds cnt ss]

-- Handles the binds of the in statements
-- variables existing in the patterns will be introduced within a Local
-- ss are statements passed from original inStatemnet that should be nested
--   within inner most local after all variables have been declares.
bindsH :: [(Patt, Exp)] -> Int -> [Stmt] -> [Stmt]
bindsH [] cnt ss = ss
bindsH ((patt,ex):binds) cnt ss = fullB
-- vars is all variables needed for current bind
-- bidnStmts are statements used in binding 
  where
    -- introVars = variables existing in pattern
    -- pattVars = variables introduced to create pattern
    -- pattBinds = binds used to create pattern
    -- mainPattVar = variable bound to pattenr record
    (introVars, pattVars, pattBinds, mainPattVar) = pattH cnt patt
    -- expBinds = binds used to create expresion ++ bind pattern to expression
    expBinds = expH cnt ex mainPattVar 
    vars = introVars ++ pattVars
    bindStmts = pattBinds ++ expBinds
    fullB = if vars == [] then bindStmts ++ bindsH binds cnt ss
      else [Local vars $ bindStmts ++ bindsH binds cnt ss]

-- Create delcarations and pass in inExpression statements, final statement will
--  be a bind between the expression and and input expID (identifier to be bound)
inExpH :: InExp -> Int-> String -> [Stmt]
inExpH (Just decPart, fstmts, ex) cnt expID 
  = decPartH decPart cnt $ elab fstmts cnt ++ expH cnt ex expID 
inExpH (Nothing, fstmts, ex) cnt expID 
  = elab fstmts cnt ++ expH cnt ex expID

-- For translating operation symbols to procedure names recognized by interpreter
opPairs :: [(String,String)]
opPairs = [("+","IntPlus"),("-","IntMinus"),("*","IntMultiply"),("/","Divide"),("<","LT"),(">","GT"),
  ("div","DivideInt"),("mod","Mod"),("==","Eq"),("\\=","NotEqual"),("=<","LessThanEq"),("=>","GreaterThanEq")]

-- Recursive function on expression type, uses the cnt to create new unique variables,
-- takes in an expression and identifier (id2bind) which is bound to the expression.
-- Returns a list of statements that produce the deisred binding.
-- Follows elab closely except in certain cases where inStmt is replaced by inExpr.
expH :: Int -> Exp -> String -> [Stmt]
expH cnt ex id2bind = case ex of
  ENum (FPInt x) -> [EqVal id2bind (PInt x)]
  ENum (FPFloat x) -> [EqVal id2bind (PFloat x)]
  EVar exID -> [EqVar id2bind exID]
  EList exps -> [Local exIDs (exStmts ++ listStmts)]
    where
      exIDs = ["EXU" ++ show (cnt+i)| i<-[1..(length exps)]]
      exStmts = concat $ map (\(a,b) -> (expH (cnt + length exps)) a b) $ zip exps exIDs 
      listStmts = expH  (cnt + length exps) (listbuild (head exIDs) (tail exIDs)) id2bind
  ERcd label featNexps -> fullK
    where 
      exIDs = ["EXU" ++ show (cnt+i)| i<-[1..(length exps)]]
      (feats, exps) = unzip featNexps
      exStmts = concat $ map (\(a,b) -> (expH (cnt+length exps)) a b) $ zip exps exIDs 
      lRcd = PRec (label, zip feats exIDs)
      bnd = EqVal id2bind lRcd
      fullK = if exIDs == [] then exStmts ++ [bnd]
        else [Local exIDs (exStmts ++ [bnd])]
  EParen ex1 ex2 "#" -> expH cnt (ERcd "'#'" [("1",ex1),("2",ex2)]) id2bind
  EParen ex1 ex2 "|" -> expH cnt (ERcd "'|'" [("1",ex1),("2",ex2)]) id2bind
  EParen ex1 ex2 op -> [Local [ex1ID, ex2ID] (ex1Stmts ++ ex2Stmts ++ [Apply  opName [ex1ID, ex2ID, id2bind]])]
    where 
      (ex1ID,ex2ID) = ("EXU" ++ show (cnt+1),"EXU" ++ show (cnt+2))
      (ex1Stmts, ex2Stmts) = (expH (cnt+3) ex1 ex1ID, expH (cnt+3) ex2 ex2ID)
      opName = head [name | (o,name)<-opPairs , o == op] -- lookup operation name
  EFunApp funID params -> fullK
    where
      exIDs = ["EXU" ++ show (cnt+i)| i<-[1..(length params)]]
      exStmts = concat $ map (\(a,b) -> (expH (cnt+length params)) a b) $ zip params exIDs
      applyStmt = Apply funID (exIDs++[id2bind])
      fullK = if exIDs == [] then exStmts ++ [applyStmt]
        else [Local exIDs  (exStmts ++ [applyStmt])]
  EFun params inExp -> [EqVal id2bind (PProc pParams pBody)]
    where
      exID = "EXU" ++ show (cnt+1)
      pParams = params ++ [exID]
      pBody = inExpH inExp (cnt+1) exID
  EProc params inStmt -> [EqVal id2bind (PProc params (inStmtH cnt inStmt))]
  ELocal inExp -> inExpH inExp cnt id2bind -- Local will come from the inExpression
  EIf ex inExp [] mElse -> fullK
    where
      exID = "EXU" ++ show (cnt+1)
      exStmts = expH (cnt+1) ex exID
      inExps = inExpH inExp (cnt+1) id2bind 
      oElse = case mElse of 
        Nothing -> [Skip Basic]
        Just (inExp) -> inExpH inExp (cnt+1) id2bind 
      ifStmt = If exID inExps oElse
      fullK = [Local [exID] (exStmts ++ [ifStmt])]
  EIf ex inExp ((ex2,inStmt2):elseIfs) mElse -> fullK
    where
      exID = "EXU" ++ show (cnt+1)
      exStmts = expH (cnt+1) ex exID
      inExps = inExpH inExp (cnt+1) id2bind 
      nestStmts = expH (cnt+1) (EIf ex2 inStmt2 elseIfs mElse) id2bind
      ifStmt = If exID inExps nestStmts
      fullK = [Local [exID] (exStmts ++ [ifStmt])]
  ECase id1 patt inExp [] mElse -> fullK
    where
      (p1,p2,p3,p4) = pattH cnt patt
      recLit = case (last p3) of 
        (EqVal _ (PRec x)) -> x
        _ -> error "case called on non record pattern"
      oElse = case mElse of 
        Nothing -> [Skip Basic]
        Just (inExp) -> inExpH inExp cnt id2bind
      caseStmt = Case id1 recLit (inExpH inExp cnt id2bind) oElse
      fullK = if (init p2) == [] then init p3 ++ [caseStmt]
        else [Local (init p2) (init p3 ++ [caseStmt])]
  ECase id1 patt inExp ((patt2,inStmt2):opts) mElse -> fullK
    where
      (p1,p2,p3,p4) = pattH cnt patt
      recLit = case (last p3) of 
        (EqVal _ (PRec x)) -> x
        _ -> error "case called on non record pattern"
      nestStmts = expH cnt (ECase id1 patt2 inStmt2 opts mElse) id2bind
      caseStmt = Case id1 recLit (inExpH inExp cnt id2bind) nestStmts
      fullK = if (init p2) == [] then init p3 ++ [caseStmt]
        else [Local (init p2) (init p3 ++ [caseStmt])]
  EThread inExp -> [Thread (inExpH inExp cnt id2bind)]
  ENewCell ex -> fullK
    where
      exID = "NCU" ++ show (cnt+1)
      exStmts = expH (cnt + 1) ex exID
      newCell = [NewCell exID id2bind] -- Ordering
      fullK = [Local [exID] (exStmts ++ newCell)]
  EAtCell id1 -> [Exchange id1 id2bind id2bind]

-- Input : cnt for naming unique variables, pattern
-- Return : (introVars, pattVars, pattBinds, mainPattVar)
    -- introVars   = variables existing in pattern
    -- pattVars    = variables introduced to create pattern
    -- pattBinds   = binds used to create pattern
    -- mainPattVar = variable bound to pattenr record
--  In all cases but PtVar, last pattVars  == mainPattVar, and 
--    last pattBinds == (mainPattVar = record)
--  SO the resulting record can be extracted by pattern matching against 
--     last pattBinds. This is done in case statements!
pattH :: Int -> Patt -> ([String], [String], [Stmt], String)
pattH cnt patt = case patt of 
  PtVar v -> ([v],[], [], v) 
  --PtAtom atom -> ([], [""], [EqVal (PVal "" (PRecLit (atom,[])))], "")
  --PtNum (FPInt x) -> ([], [""], [EqVal (PVal "" (PInt x))], "")
  --PtNum (FPFloat x) -> ([], [""], [EqVal (PVal "" (PFloat x))], "")
  PtList patts -> (concat p1s, concat p2s++["PTU"++show cnt], concat p3s++lstmts, "PTU"++show cnt)
    where 
      (p1s,p2s,p3s,p4s) = unZip4 $ map (\(a,b) -> pattH a b) $ zip [cnt+i| i<-[1..(length patts)]] patts
      lstmts = expH (cnt+1) (listbuild (head p4s) (tail p4s)) ("PTU"++show cnt)
  PtParen p1 p2 op -> (p11++p21 ,p12++p22++["PTU"++show cnt] ,p13++p23++[bnd], "PTU"++show cnt)
    where 
      (p11, p12, p13, p14) = pattH (cnt+1) p1
      (p21, p22, p23, p24) = pattH (cnt+2) p2
      pRcd = PRec ('\'':op:'\'':[], [("1",p14),("2",p24)])
      bnd = EqVal ("PTU"++show cnt) pRcd
  PtRec label featNpatts -> (concat p1s, concat p2s++["PTU"++show cnt], concat p3s++[bnd], "PTU"++show cnt)
    where 
      (feats, patts) = unzip featNpatts
      (p1s,p2s,p3s,p4s) = unZip4 $ map (\(a,b) -> pattH a b) $ zip [cnt+i| i<-[1..(length patts)]] patts
      lRcd = PRec (label, zip feats p4s)
      bnd = EqVal ("PTU"++show cnt) lRcd

-- Build a list as an expression record, then use the expression helper to
-- convert to a record in the kernel syntax.
listbuild :: String -> [String] -> Exp
listbuild h [] = ERcd "'|'" [("1",EVar h),("2",ERcd "nil" [])]
listbuild h (t:ts) = ERcd "'|'" [("1",EVar h),("2",listbuild t ts)]

unZip4 [] = ([],[],[],[])
unZip4 ((a,b,c,d):l) = (a:as,b:bs,c:cs,d:ds)
  where
    (as,bs,cs,ds) = unZip4 l

