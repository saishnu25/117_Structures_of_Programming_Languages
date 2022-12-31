-- kernel.hs
-- Defines the kernel syntax data type, along with some basic syntactic support
--   like free-variable determination and program output
-- Imports: 

module Kernel where

import Data.List

{---- Oz Kernel Syntax ----------------------

In this simplified Oz syntax, record labels and features are atoms (strings),
where we use "1", "2", etc., for integer features and "true" and "false" for
Boolean features, sequences of statements are represented by lists, and the
skip statement doubles as a state-output statement.  -}

type Var    = String                       -- Program variables
type Atom   = String                       -- Atoms
type RecLit = (Atom, [(Atom,Var)])         -- Record literals (label, feat/vars)

-- Program values
data PVal = PInt Int                       -- Integer literals
          | PFloat Float                   -- Floating-point literals
          | PRec RecLit                    -- Record literals
          | PProc [Var] [Stmt]             -- Procedures (args, body)
         
-- Statements
data Stmt = Skip SkipType                  -- Empty statement (optional output)
          | Thread [Stmt]                  -- Thread introduction
          | Local [Var] [Stmt]             -- Variable introduction (multiple)
          | EqVar Var Var                  -- Variable-variable binding
          | EqVal Var PVal                 -- Variable-value binding
          | If Var [Stmt] [Stmt]           -- Conditional statement
          | Case Var RecLit [Stmt] [Stmt]  -- Pattern matching
          | NewName Var                    -- Name introduction
          | Apply Var [Var]                -- Procedure application
          | IsDet Var Var                  -- State: determined?
          | NewCell Var Var                -- State: new cell
          | Exchange Var Var Var           -- State: exchange cells
          | ByNeed Var Var                 -- Laziness: by-need trigger
          | TryCatch [Stmt] Var [Stmt]     -- Exception handling: try/catch
          | Raise Var                      -- Exception handling: raise

data SkipType = Basic                      -- Ordinary skip
              | Store                      -- Skip and display store
              | Full                       -- Skip and display environ + store
              | Check                      -- Skip and check state invariants
              | Browse String
              | Stack


-- Free variables in program values and statements (without duplicates)
fvar_pval :: PVal -> [Var]
fvar_pval (PInt _) = []
fvar_pval (PFloat _) = []
fvar_pval (PRec (lab,fvs)) = nub $ map snd fvs
fvar_pval (PProc vs ms) = fvar_stmts ms \\ vs

fvar_stmt :: Stmt -> [Var]
fvar_stmt (Skip _) = []
fvar_stmt (Thread ms) = fvar_stmts ms
fvar_stmt (Local vs ms) = fvar_stmts ms \\ vs
fvar_stmt (EqVar v1 v2) = nub [v1,v2]
fvar_stmt (EqVal v1 pv) = union [v1] (fvar_pval pv)
fvar_stmt (If v1 ms1 ms2) = union (fvar_stmts ms1) (v1:fvar_stmts ms2)
fvar_stmt (Case v1 (lab,fvs) ms1 ms2) =
  union (fvar_stmts ms1 \\ map snd fvs) (v1:fvar_stmts ms2)
fvar_stmt (NewName v1) = [v1]
fvar_stmt (Apply v1 vs) = nub (v1:vs)
fvar_stmt (IsDet v1 v2) = nub [v1,v2]
fvar_stmt (NewCell v1 v2) = nub [v1,v2]
fvar_stmt (Exchange v1 v2 v3) = nub [v1,v2,v3]
fvar_stmt (ByNeed v1 v2) = nub [v1,v2]
fvar_stmt (TryCatch ms1 v1 ms2) = union (fvar_stmts ms1) (v1:fvar_stmts ms2)
fvar_stmt (Raise v1) = [v1]

fvar_stmts :: [Stmt] -> [Var]
fvar_stmts ms = foldl' (\xs stm -> union xs (fvar_stmt stm)) [] ms


{---- Program output ----------------}

-- Convert list of strings to a string, using delimiters
l2s :: String -> String -> String -> [String] -> String
l2s beg mid end [] = beg ++ end
l2s beg mid end [xs] = beg ++ xs ++ end
l2s beg mid end (xs:yss) = beg ++ xs ++ concatMap (mid++) yss ++ end

show_rec :: RecLit -> String
show_rec (a,fvs) = a ++ l2s "(" " " ")" (map (\(a,v) -> a ++ ":" ++ v) fvs)

instance Show PVal where
  show (PInt n) = show n
  show (PFloat f) = show f
  show (PRec rl) = show_rec rl
  show (PProc vs stmts) = "proc " ++ l2s "{$ " " " "}" vs ++ " " ++ show stmts

instance Show Stmt where
  show (Skip Basic) = "skip"
  show (Skip Store) = "skip/s"
  show (Skip Full)  = "skip/f"
  show (Skip Check) = "skip/c"
  show (Skip (Browse s)) = "skip/B"++s
  show (Skip Stack) = "skip/st"
  show (Thread stmts) = "thread " ++ show stmts
  show (Local vs stmts) = "local " ++ show vs ++ " " ++ show stmts -- l2s "" " " "" vs ++ " " ++ show stmts
  show (EqVar v1 v2) = v1 ++ " = " ++ v2
  show (EqVal v1 val) = v1 ++ " = " ++ show val
  show (If v trues falses)
    = "if " ++ v ++ " then " ++ show trues ++ " else " ++ show falses
  show (Case v rl thens elses)
    = "case " ++ v ++ " of " ++ show_rec rl ++ " then " ++ show thens ++
      " else " ++ show elses
  show (NewName v) = "{NewName " ++ v ++ "}"
  show (Apply f ps) = show f ++ (concat $ map (\x->" "++show x) ps)
  show (IsDet v1 v2) = "{IsDet " ++ v1 ++ " " ++ v2 ++ "}"
  show (NewCell v1 v2) = "{NewCell " ++ v1 ++ " " ++ v2 ++ "}"
  show (Exchange v1 v2 v3) = "{Exchange " ++ v1 ++ " " ++ v2 ++ " " ++ v3 ++ "}"
  show (ByNeed v1 v2) = "{ByNeed " ++ v1 ++ " " ++ v2 ++ "}"
  show (TryCatch trys v catch)
    = "try " ++ show trys ++ " catch " ++ v ++ " then " ++ show catch
  show (Raise v) = "raise " ++ v