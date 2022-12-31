-- full.hs
-- Defines the full language syntax data type and some syntactic support
--   analogously to kernel.hs with duplicated names denoted with 'F' for full

module Full where

{---- Oz Full Syntax ----------------------}

type FVar    = String                       -- Program variables
type FAtom   = String                       -- Atoms

-- Program Numbers
data FPNum = FPInt Int                       -- Integer literals
          | FPFloat Float                    -- Floating-point literals

-- Patterns (Variables included for inStmt binding purposes, but will not work
-- for case statements)
data Patt = PtVar FVar
      | PtRec FAtom [(FAtom, Patt)]           -- pattern record
      | PtList [Patt]                         -- pattern list
      | PtParen Patt Patt Char                -- character is either '#' or '|'


-- Expressions
data Exp = ENum FPNum                         -- Number
     | EVar FVar                              -- variable
     | EList [Exp]                            -- expression list
     | ERcd FAtom [(FAtom,Exp)]               -- expression record
     | EFunApp FVar [Exp]                     -- function application
     | EFun [FVar] InExp                      -- function definition
     | EProc [FVar] InStmt                    -- procedure definition
     | ELocal InExp                           -- local
     | EIf Exp InExp [(Exp,InExp)] (Maybe InExp)  -- if
     | ECase FVar Patt InExp [(Patt, InExp)] (Maybe InExp) -- case
     | EThread InExp                          -- thread
     | ENewCell Exp                           -- NewCell
     | EAtCell FVar                           -- @ to extract value from cell
     | EParen Exp Exp String                  -- string is the operation

type DecPart = ([FVar], [(Patt,Exp)])         -- List of variables and bindings


type InStmt = (Maybe DecPart,[FStmt])         -- Maybe declations and statements

type InExp = (Maybe DecPart, [FStmt], Exp)    -- Maybe declarations, statements and expression

data BVal = Ex Exp | Pr [FVar] InStmt | Fn [FVar] InExp        
-- Binding either expression or procedure or function


-- Statements
data FStmt = FSkip FSkipType                  -- Empty statement (optional output)
          | FThread InStmt                    -- Thread introduction
          | FLocal InStmt                     -- Variable introduction (multiple)
          | FEq Patt BVal                     -- Binding pattern to expression
          | FIf Exp InStmt [(Exp,InStmt)] (Maybe InStmt)   -- Conditional statement
          | FCase FVar Patt InStmt [(Patt, InStmt)] (Maybe InStmt)  -- Pattern matching
          -- | FNewName FVar                  -- Name introduction
          | FApply FVar [Exp]                 -- Procedure application
          -- | FIsDet FVar FVar               -- State: determined?
          | FNewCell Exp FVar                 -- State: new cell
          | FExchange FVar FVar Exp           -- State: exchange cells
          | FSetCell FVar Exp                 -- state: Set Cell Value
          | FByNeed Exp FVar                  -- Laziness: by-need trigger
          -- | FTryCatch [FStmt] FVar [FStmt]     -- Exception handling: try/catch
          -- | FRaise FVar                      -- Exception handling: raise

data FSkipType = FBasic                     -- Ordinary skip
              | FStore                      -- Skip and display store
              | FFull                       -- Skip and display environ + store
              | FCheck                      -- Skip and check state invariants
              | FBrowse String              -- Skip and display variable
              | FStack                      -- skip and display Stack


{---- Program output ----------------}
-- Used for debgugging purposes but is not traced in Hoz before elaboration

-- Convert list of strings to a string, using delimiters
l2s :: String -> String -> String -> [String] -> String
l2s beg mid end [] = beg ++ end
l2s beg mid end [xs] = beg ++ xs ++ end
l2s beg mid end (xs:yss) = beg ++ xs ++ concatMap (mid++) yss ++ end

--show_rec :: RecLit -> String
--show_rec (a,fvs) = a ++ l2s "(" " " ")" (map (\(a,v) -> a ++ ":" ++ v) fvs)

instance Show FPNum where
  show (FPInt n) = show n
  show (FPFloat f) = show f

instance Show BVal where
  show (Pr vs inStmt) = "proc " ++ l2s "{$ " " " "}" vs ++ " " ++ show inStmt
  show (Fn vs inStmt) = "proc " ++ l2s "{$ " " " "}" vs ++ " " ++ show inStmt
  show (Ex e) = show e

instance Show FStmt where
  show (FSkip FBasic) = "skip"
  show (FSkip FStore) = "skip/s"
  show (FSkip FFull)  = "skip/f"
  show (FSkip FCheck) = "skip/c"
  show (FSkip (FBrowse s)) = "skip/B"++s
  show (FSkip FStack) = "skip/st"
  show (FThread stmts) = "thread " ++ show stmts
  show (FLocal stmts) = "local " ++ show stmts
  show (FEq v1 v2) = show v1 ++ " = " ++ show v2
  show (FIf ex inStmt [] melse)
    = "if " ++ show ex ++ " then " ++ show inStmt ++ " else " ++ show melse
  show (FIf ex inStmt ((ex2,inStmt2):elseIfs) melse)
    = "if " ++ show ex ++ " then " ++ show inStmt ++ " else" ++ show (FIf ex2 inStmt2 elseIfs melse)
  show (FCase id1 patt inStmt [] mElse)
    = "case " ++ id1 ++ " of " ++ show patt ++ " then " ++ show inStmt ++
      " else " ++ show mElse
  show (FCase id1 patt inStmt ((patt2,inStmt2):opts) mElse)
    = "case " ++ id1 ++ " of " ++ show patt ++ " then " ++ show inStmt ++ " [] " ++
      show (FCase id1 patt2 inStmt2 opts mElse)
  -- show (FNewName v) = "{NewName " ++ show v ++ "}"
  show (FApply f ps) = show f ++ (concat $ map (\x->" "++show x) ps)
  -- show (FIsDet v1 v2) = "{IsDet " ++ v1 ++ " " ++ v2 ++ "}"
  show (FNewCell v1 v2) = "{NewCell " ++ show v1 ++ " " ++ v2 ++ "}"
  show (FExchange v1 v2 v3) = "{Exchange " ++ v1 ++ " " ++ v2 ++ " " ++ show v3 ++ "}"
  show (FByNeed v1 v2) = "{ByNeed " ++ show v1 ++ " " ++ v2 ++ "}"
  --show (TryCatch trys v catch)
  --  = "try " ++ show trys ++ " catch " ++ v ++ " then " ++ show catch
  --show (Raise v) = "raise " ++ v
  show (FSetCell v ex) = v ++ show ex


instance Show Exp where
  show (ENum s) = show s
  show (EVar s) = show s
  show (EList es) = (concat $ map (\x->" "++show x) es)
  show (ERcd l fs) = show l ++ " " ++ show fs
  show (EParen e1 b e2) = show e1 ++ " " ++ show b ++ " " ++ show e2
  show (EFunApp f ps) = show f ++ " " ++ (concat $ map (\x->" "++show x) ps)
  show (EFun params inExp) = show params ++ show inExp
  show (EProc params inStmt) = show params ++ show inStmt
  show (EIf ex inStmt [] melse)
    = "if " ++ show ex ++ " then " ++ show inStmt ++ " else " ++ show melse
  show (EIf ex inStmt ((ex2,inStmt2):elseIfs) melse)
    = "if " ++ show ex ++ " then " ++ show inStmt ++ " else" ++ show (EIf ex2 inStmt2 elseIfs melse)
  show (ECase id1 patt inStmt [] mElse)
    = "case " ++ id1 ++ " of " ++ show patt ++ " then " ++ show inStmt ++
      " else " ++ show mElse
  show (ECase id1 patt inStmt ((patt2,inStmt2):opts) mElse)
    = "case " ++ id1 ++ " of " ++ show patt ++ " then " ++ show inStmt ++ " [] " ++
      show (ECase id1 patt2 inStmt2 opts mElse)
  show (EThread stmts) = "thread " ++ show stmts
  show (ELocal stmts) = "local " ++ show stmts
  show (ENewCell exp) = show exp
  show (EAtCell c) = show c

instance Show Patt where
  show (PtVar s) = show s
  show (PtRec s fs) = show s ++ " " ++ show fs
  show (PtParen e1 b e2) = show e1 ++ " " ++ show b ++ " " ++ show e2
  show (PtList ps) = (concat $ map (\x->" "++show x) ps)







