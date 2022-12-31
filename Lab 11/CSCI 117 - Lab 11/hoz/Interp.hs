-- interp.hs
-- Defines the semantic data types and functions, exporting high-level 
--   interpretation functions
-- Imports: kernel.hs

module Interp where

import Kernel
import Debug.Trace
import Data.List
  
                                                        
{---- Oz Semantics ----------------}

type SLoc = Int                            -- Store locations
type Name = Int                            -- Names
type Env = [(Var,SLoc)]                    -- Environments
type RecVal = (Atom, [(Atom,SLoc)])        -- Record values (label, feats/vals)
type Clos = ([Var], [Stmt], Env)           -- Closures (args, body, cenv)



-- Primitive operations take the current state (defined below) and list of
-- arguments given as store IDs and return either an error message or the
-- updated state.
type PrimOp = State -> [SLoc] -> ExecResult

intPrimOp :: (Int->Int->Int) -> State -> [SLoc] -> ExecResult
intPrimOp op s@State{sas=sas,newlocs=newids} [s1,s2,s3] = case slookup s1 sas of
  Nothing -> EError $ "Impossible: store missing id " ++ show s1
  Just (_,SUnbound) -> ESuspend
  Just (_,SThunk id1) -> ENeed id1 s1
  Just (_,SInt x) -> case slookup s2 sas of
    Nothing -> EError $ "Impossible: store missing id " ++ show s2
    Just (_,SUnbound) -> ESuspend
    Just (_,SThunk id2) -> ENeed id2 s2
    Just (_,SInt y) -> case slookup s3 sas of
      Nothing -> EError $ "Impossible: store missing id " ++ show s3
      Just (_,SUnbound) ->
        let sas' = supdate sas s3 (SInt (op x y))        -- Short cut: reuse
        in ENext s{sas = sas'}               -- unbound variable
      Just (_,SThunk id3) -> ENeed id3 s3
      Just _ ->
        let id2:rest = newids   -- Get new store id
        in case bind (([id2],(SInt (op x y))):sas) s3 id2 of
             Nothing ->
               EError $ "Can't unify " ++ show (SInt (op x y)) ++ " in value creation"
             Just sas'' ->
               ENext s{sas = sas'', newlocs = rest}
    _ -> EError $ "Not an Int " ++ show s2 ++ show (op 1 2)
  _ -> EError $ "Not an Int " ++ show s1 ++ show (op 1 2)
intPrimOp _ _ _ = EError "Incorrect number of arguments on int primitive operation (expected 3)"

int_plus = intPrimOp (+) -- Currently IntPlus in environment

int_minus = intPrimOp (-) -- Currently IntMinus in environment

int_multiply = intPrimOp (*)

intMod = intPrimOp (mod)

int2BoolPrimOp :: (Int->Int->Bool) -> State -> [SLoc] -> ExecResult
int2BoolPrimOp op s@State{sas=sas,newlocs=newids} [s1,s2,s3] = case slookup s1 sas of
  Nothing -> EError $ "Impossible: store missing id " ++ show s1
  Just (_,SUnbound) -> ESuspend
  Just (_,SThunk id1) -> ENeed id1 s1
  Just (_,SInt x) -> case slookup s2 sas of
    Nothing -> EError $ "Impossible: store missing id " ++ show s2
    Just (_,SUnbound) -> ESuspend
    Just (_,SThunk id2) -> ENeed id2 s2
    Just (_,SInt y) -> case slookup s3 sas of
      Nothing -> EError $ "Impossible: store missing id " ++ show s3
      Just (_,SUnbound) ->
        let sas' = supdate sas s3 (SRec (b2b (op x y), []))        -- Short cut: reuse
        in ENext s{sas = sas'}               -- unbound variable
      Just (_,SThunk id3) -> ENeed id3 s3
      Just _ ->
        let id2:rest = newids   -- Get new store id
        in case bind (([id2],(SRec (b2b (op x y),[]))):sas) s3 id2 of
             Nothing ->
               EError $ "Can't unify " ++ show (SRec (b2b (op x y),[])) ++ " in value creation"
             Just sas'' ->
               ENext s{sas = sas'', newlocs = rest}
    _ -> EError $ "Not an Int " ++ show s2
  _ -> EError $ "Not an Int " ++ show s1
int2BoolPrimOp _ _ _ = EError "Incorrect number of arguments on int primitive operation (expected 3)"

b2b False = "false"
b2b True = "true"

intGT = int2BoolPrimOp (>)
intLT = int2BoolPrimOp (<)


equality_check :: PrimOp -- Currently Eq in environment
equality_check s@State{sas=sas,newlocs=newids} [s1,s2,s3] = case slookup s1 sas of
  Nothing -> EError $ "Impossible: store missing id " ++ show s1
  Just (_,v1) -> case slookup s2 sas of
    Nothing -> EError $ "Impossible: store missing id " ++ show s2
    Just (_,v2) -> case eq v1 v2 s1 s2 of
      Left b -> case slookup s3 sas of
        Nothing -> EError $ "Impossible: store missing id " ++ show s3
        Just (_,SUnbound) ->
          let sas' = supdate sas s3 (toR b)        -- Short cut: reuse
          in ENext s{sas = sas'}               -- unbound variable
        Just (_,SThunk id3) -> ENeed id3 s3
        Just _ ->
          let id2:rest = newids   -- Get new store id
          in case bind (([id2],toR b):sas) s3 id2 of
               Nothing ->
                 EError $ "Can't unify " ++ show (toR b) ++ " in value creation"
               Just sas'' ->
                 ENext s{sas = sas'', newlocs = rest}
      Right s' -> s'
  where
    toR bl = if bl then SRec ("true",[]) else SRec ("false",[])
    eq :: SVal -> SVal -> SLoc -> SLoc -> Either Bool ExecResult
    eq (SInt x) (SInt y) _ _ = Left $ x == y
    eq (SFloat x) (SFloat y) _ _ = Left $ x == y
    eq (SRec (l1,f1)) (SRec (l2,f2)) _ _ = if l1 == l2 
      then Left True -- Needs to be implemented 
                     --   - Check feature values exist in store (error otherwise)
                     --   - Map eq over feature values pairs
                     --     - Left -> and bool list
                     --     - Right -> return first response (ESuspend or ENeed)
      else Left False
    eq (SName n1) (SName n2) _ _ = Left $ n1 == n2
    eq (SCell n1) (SCell n2) _ _ = Left $ n1 == n2
    eq (SThunk n1) (SThunk n2) _ _ = Left $ n1 == n2 -- need to evaluate if different
    eq (SUnbound) (SUnbound) _ _ = Left True
    eq (SUnbound) _ _ _= Right ESuspend
    eq _ (SUnbound) _ _= Right ESuspend
    eq (SThunk n1) _ i _ = Right $ ENeed n1 i
    eq _ (SThunk n1) _ i = Right $ ENeed n1 i 
    eq _ _ _ _= Right $ EError "eq could not compare inputs"
equality_check _ _ = EError "Incorrect number of arguments on equality_check (expected 3)"



-- Store values
data SVal = SUnbound                       -- Unbound
          | SInt Int                       -- Integer value
          | SFloat Float                   -- Floating-point value
          | SRec RecVal                    -- Record value
          | SProc Clos                     -- Procedure value
          | SPrimOp PrimOp                 -- Primitive (built-in) operation
          | SName Name                     -- Name value
          | SCell Name                     -- Cell name (-> mutable store)
          | SThunk SLoc                    -- Thunk reference

show_Srec :: RecVal -> String
show_Srec (a,fvs) = a ++ l2s "(" " " ")" (map (\(a,v) -> a ++ ":" ++ (show v)) fvs)

instance Show SVal where
  show SUnbound = "Unbound"
  show (SInt n) = show n
  show (SFloat f) = show f
  show (SRec rl) = show_Srec rl
  show (SProc clos) = "proc" ++ show clos
  show (SPrimOp op) = "Primitive Operation"
  show (SName n) = "Name "++(show n)
  show (SCell n) = "Cell "++(show n)
  show (SThunk s) = "Thunk "++(show s)

type Stack = [(Stmt, Env)]                 -- Execution stack of semantic stmts
type Thread = (Int, Stack)                 -- Thread (thread #, stack)
type SAS = [([SLoc],SVal)]                 -- Single-assignment store
type MUT = [(Name,SLoc)]                   -- Mutable store

data State = State {
  front :: [Thread],                       -- Queue of threads, front
  back :: [Thread],                        -- Queue of threads, reversed back

  sas :: SAS,                              -- Single-assignment store
  muts :: MUT,                             -- Mutable store 

  newlocs :: [Int],                        -- List of new SLocs (subject to GC)
  newname :: Int,                          -- Next available name
  newthread :: Int                         -- Next available thread number
}

{- Program state. Quey of waiting threads is a functional queue data structure
split into front and reversed back for efficiency (see Section 3.4.5 of CTM).
State invariants:
  * first components of front and back (thread #s) are all positive, less than
    newthread, and have no duplicates
  * first components of sas (var groups) are nonempty, pairwise disjoint, have
    only positive elements, and have no duplicates; let SLOCS be their union
  * first components of muts are positive, less than newname, and distinct
  * every variable in every environment in front, back, and sas (inside
    closures) refers to an actual store location -- i.e., one in SLOCS
  * every store location in sas (in a record value or thunk) is in SLOCS
  * every name in sas (as a name value or cell reference) exists in muts
  * no single name in the store is used for more than one purpose (name, cell)
  * newlocs is an infinite list with no duplicates; newname, newthread >= 1
  * record values in the store have their features sorted and distinct.

Violations of these invariants produce an "Impossible" error message in the
best case and undefined behavior in the worst case. -}

-- Check for duplicates
dups :: Eq a => [a] -> Bool
dups [] = False
dups (x:xs) = elem x xs || dups xs

-- Check for sortedness and distinctness
sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:xs@(y:ys)) = x < y && sorted xs


-- Check state invariants; used for illustration and debugging purposes
state_check :: State -> Bool
state_check s@State{front = front, back = back, sas = sas, muts = muts,
                    newname = newname, newthread = newthread} =
  let thnos = map fst $ front ++ back        -- thread numbers
      slocs = concatMap fst sas              -- SAS locations
      svals = map snd sas                    -- SAS values
      mutnames = map fst muts                -- cell names
      env_ids = concatMap (map snd) $        -- ids from enviroments ...
                 map snd (concatMap snd $ front ++ back)  -- in stacks
                 ++ [ce | SProc (_,_,ce) <- svals]        -- in closures
      rec_ids = concat [map snd fvs | SRec (_,fvs) <- svals] -- ids from recs
      thunk_ids = [id | SThunk id <- svals]  -- ids from thunks
      sas_names = [n | SName n <- svals]     -- names from SAS
      sas_cells = [n | SCell n <- svals]     -- cell names from SAS
      nodups = not . dups
  in all (\tn -> 0 < tn && tn < newthread) thnos && nodups thnos
     && all (not . null) (map fst sas) && all (>0) slocs && nodups slocs
     && all (\n -> 0 < n && n < newname) mutnames && nodups mutnames
     && all (`elem` slocs) (env_ids ++ rec_ids ++ thunk_ids)
     && all (`elem` mutnames) sas_cells && null (intersect sas_names sas_cells)
     {- can't check for infiniteness of or lack of duplicates in newlocs -}
     && all sorted [map fst fvs | SRec (l,fvs) <- svals] 


---- Semantic helper functions ----------------

tplSec x y = (y,x)

-- Output current state (not yet implemented)
output_state :: Env -> MUT ->  SkipType -> State -> ()
output_state e muts (Basic) s = ()
output_state e muts (Check) s = trace ("State Invariant Check : "++(show $ state_check s)++"\n") ()
output_state e muts (Store) s = trace ("Store : "++(store2string (sas s))++"\n") ()
output_state e muts (Full) s = trace ("Store : "++(store2string (sas s))++"\n"++
    "Mutable Store: " ++ (mut2string muts) ++ "\n" ++
    "Current Environment : "++(env2string e)++"\n" ++
    "Stack : "++ show st ++ "\n") ()
  where st = if (null.head.front) s then [] else concat $ map (\(x,y)->show x) $ (snd.head.front) s
output_state e muts (Browse id) s = case check_vars e [id] of
  Left s -> error $ "Browse on invalid id " ++ id
  Right [sl] -> case slookup sl (sas s) of
    Nothing -> error $ "Browse on invalid id " ++ id
    Just (_,SRec ("'|'",[("1",h),("2",t)])) -> trace (id ++ " : [" ++ head2string h (sas s)++ list2string t (sas s) ++" ]\n") ()
    Just (_,v) -> trace (id ++ " : " ++ show v ++ "\n") ()
output_state e muts (Stack) s = trace ("Stack : "++ show st ++ "\n") ()
  where st = if (null.head.front) s then [] else concat $ map (\(x,y)->show x) $ (snd.head.front) s

head2string sloc sas = case slookup sloc sas of
  Nothing -> error $ "Browse on invalid list head id " ++ show sloc
  Just (_,v) -> " "++show v

list2string sloc sas = case slookup sloc sas of
  Nothing -> error $ "Browse on invalid list tail id " ++ show sloc
  Just (_,SRec ("'|'",[("1",h),("2",t)])) -> " "++head2string h sas++list2string t sas
  Just (_,SRec ("nil",[])) -> []
  Just (_,v) -> " "++show v


store2string :: SAS -> String
store2string [] = "Empty"
store2string [((x:xs),sv)] = "(("++(show x)++(concat(map (\x->", "++(show x)) xs))++"), "++(show sv)++")\n"
store2string (((x:xs),sv):sas) = "(("++(show x)++(concat(map (\x->", "++(show x)) xs))++"), "++(show sv)++"), \n"++(store2string sas)

env2string :: Env -> String
env2string [] = "Empty"
env2string ((v1,s1):e) = "("++(show v1)++" -> "++(show s1)++(concat(map (\(x,y)->", "++(show x)++" -> "++(show y)) e))++")"

mut2string :: MUT -> String
mut2string [] = "Empty"
mut2string ((v1,s1):e) = "("++(show v1)++" -> "++(show s1)++(concat(map (\(x,y)->", "++(show x)++" -> "++(show y)) e))++")"

-- Lookup store location in single-assignment store
slookup :: SLoc -> SAS -> Maybe ([SLoc],SVal)
slookup id [] = Nothing
slookup id ((xs,sv):sas) | elem id xs = Just (xs,sv)
                         | otherwise = slookup id sas

-- Update single-assignment store with new or replaced binding
-- Invariant: if (id,sv) is in sas, then sv == SUnbound
supdate :: SAS -> SLoc -> SVal -> SAS
supdate [] id val = [([id],val)]
supdate ((vg,sv):sas) id val | elem id vg = (vg,val):sas
                             | otherwise = (vg,sv):supdate sas id val

-- Update mutable store with replaced binding (name exists in muts)
mupdate :: MUT -> Name -> SLoc -> MUT
mupdate ((n,id):muts) n0 id0 | n0 == n = (n0,id0):muts
                             | otherwise = (n,id):mupdate muts n0 id0

-- Remove and return an existing binding group from a single-assignment store
sremove :: SAS -> SLoc -> ([SLoc],SVal,SAS)
sremove ((vg,sv):sas) id
  | elem id vg = (vg,sv,sas)
  | otherwise = let (vg',sv',sas') = sremove sas id
                in (vg',sv', (vg,sv):sas')

-- Bind two store locations (assumed to exist in sas) together, if possible
bind :: SAS -> SLoc -> SLoc -> Maybe SAS
bind sas id1 id2 = binds sas [(id1,id2)] where
  binds :: SAS -> [(SLoc,SLoc)] -> Maybe SAS
  binds sas [] = Just sas
  binds sas ((id1,id2):ids)
    | any (\(vg,sv) -> elem id1 vg && elem id2 vg) sas = binds sas ids
    | otherwise =
      let (vg1,sv1,sas1) = sremove sas id1
          (vg2,sv2,sas2) = sremove sas1 id2
          vg = vg1 ++ vg2
          sas' = (vg,sv1):sas2   -- could have used sv2 instead
          match n1 n2 = if n1 == n2 then binds sas' ids else Nothing
      in case (sv1,sv2) of
        (SUnbound, _) -> binds ((vg,sv2):sas2) ids
        (_, SUnbound) -> binds sas' ids
        (SInt n1, SInt n2) -> match n1 n2
        (SFloat f1, SFloat f2) -> match f1 f2
        (SRec (l1,fvs1), SRec (l2,fvs2)) ->
          let (fs1,ids1) = unzip fvs1
              (fs2,ids2) = unzip fvs2
          in if l1 == l2 && fs1 == fs2 then binds sas' (zip ids1 ids2 ++ ids)
             else Nothing
        (SName n1, SName n2) -> match n1 n2
        (SCell n1, SCell n2) -> match n1 n2
        (SThunk n1, SThunk n2) ->
          if any (\(vg,sv) -> elem n1 vg && elem n2 vg) sas
          then binds sas' ids
          else Nothing
        _ -> Nothing

-- Check variables and either provide error message or valid store ids
check_vars :: Env -> [Var] -> Either String [SLoc]
check_vars e vs = cv e vs [] [] False where
  cv :: Env -> [Var] -> [String] -> [SLoc] -> Bool -> Either String [SLoc]
  cv e [] msgs ids err =
    if err then Left (l2s "" "\n" "" $ reverse msgs) else Right (reverse ids)
  cv e (v:vs) msgs ids err = case lookup v e of
    Nothing -> cv e vs (("Variable " ++ v ++ " undeclared") : msgs) ids True
    Just id -> cv e vs msgs (id:ids) err

-- Create store value, if possible (return an error message if not)
create_val :: Env -> PVal -> Either String SVal
create_val e (PInt n) = Right (SInt n)
create_val e (PFloat f) = Right (SFloat f)
create_val e (PRec (lab,fvs)) =
  let fvs' = sortBy (\(a,_) (b,_) -> compare a b) fvs
      (fs',vs') = unzip fvs'
  in case check_vars e vs' of
    Left msg -> Left msg
    Right ids -> Right (SRec (lab, zip fs' ids))
create_val e pv@(PProc xs ms) =
  let frees = fvar_pval pv
  in case check_vars e frees of
    Left msg -> Left msg
    Right ids -> Right (SProc (xs, ms, zip frees ids))


-- Program Execution

data ExecResult = EError String            -- Error with message
                | ESuspend                 -- Statement suspends
                | ENeed SLoc SLoc              -- Need thunk value
                | ENext State              -- Steps to a new state

-- Execute a single statement in a single thread: exec m e s
-- m is the statement to execute
-- e is the environment in which to execute it
-- s is the current state, with the current thread on top of front and with the
--   semantic statement (m,e) removed from its stack
exec :: Stmt -> Env -> State -> ExecResult

exec (Skip skt) e s@State{muts=muts} = seq (output_state e muts skt s) (ENext s)

exec (Thread ms) e s@State{back = back, newthread = tn} =
  ENext s{back = (tn, map (tplSec e) ms) : back, newthread = tn+1}

exec (Local vs ms) e s@State{front = (tn,st):ths, sas = sas, newlocs = newids}
  | null vs = EError "No variables in local declaration"
  | dups vs = EError "Duplicate variables in local declaration"
  | otherwise =
      let (ids, rest) = splitAt (length vs) newids
          e' = zip vs ids ++ e
          st' = map (tplSec e') ms ++ st
          sas' = map (\id -> ([id],SUnbound)) ids ++ sas
      in ENext s{front = (tn,st'):ths, sas = sas', newlocs = rest}

exec (EqVar v1 v2) e s@State{sas = sas} = 
  case check_vars e [v1,v2] of
    Left msg -> EError msg
    Right [id1,id2] -> 
      case bind sas id1 id2 of
        Nothing -> EError $ "Can't unify " ++ show id1 ++ " and " ++ show id2
        Just sas' -> ENext s{sas = sas'}

exec (EqVal v1 pv) e s@State{sas = sas, newlocs = newids} =
  case check_vars e [v1] of
    Left msg -> EError msg
    Right [id1] -> 
      case create_val e pv of
        Left msg -> EError msg
        Right sv -> 
          case slookup id1 sas of
            Nothing -> EError $ "Impossible: store missing id " ++ show id1
            Just (_,SUnbound) ->
              let sas' = supdate sas id1 sv        -- Short cut: reuse
              in ENext s{sas = sas'}               -- unbound variable
            Just _ ->
              let id2:rest = newids   -- Get new store id
              in case bind (([id2],sv):sas) id1 id2 of
                   Nothing ->
                     EError $ "Can't unify " ++ v1 ++ " in value creation"
                   Just sas'' ->
                     ENext s{sas = sas'', newlocs = rest}

exec (If v1 ms1 ms2) e s@State{front = (tn,st):ths, sas = sas} = 
  case check_vars e [v1] of
    Left msg -> EError msg
    Right [id1] -> 
      case slookup id1 sas of
        Nothing -> EError $ "Impossible: store missing id " ++ show id1
        Just (_,SUnbound) -> ESuspend
        Just (_,SThunk id) -> ENeed id id1
        Just (_,SRec ("true",[]))  -> let st' = map (tplSec e) ms1 ++ st
          in ENext s{front = (tn,st'):ths}
        Just (_,SRec ("false",[])) -> let st' = map (tplSec e) ms2 ++ st
          in ENext s{front = (tn,st'):ths}
        Just _ -> EError $ "Variable " ++ v1 ++ " not bound to Boolean"

exec (Case v1 (lab,fvs) ms1 ms2) e s@State{front = (tn,st):ths, sas = sas} =
  if dups (map snd fvs) then EError $ "Duplicate variables in case pattern"
  else  case check_vars e [v1] of
    Left msg -> EError msg
    Right [id1] -> 
      case slookup id1 sas of
        Nothing -> EError $ "Impossible: store missing id " ++ show id1
        Just (_,SUnbound) -> ESuspend
        Just (_,SThunk id) -> ENeed id id1
        Just (_,SRec (lab1,fvs1))  ->
          let fvs' = sortBy (\(a,_) (b,_) -> compare a b) fvs
              (fs',vs') = unzip fvs'
              (fs1,vs1) = unzip fvs1
              st' = if lab1 == lab && fs1 == fs'  -- arities match
                    then let e' = zip vs' vs1 ++ e
                         in map (tplSec e') ms1 ++ st
                    else map (tplSec e) ms2 ++ st
          in ENext s{front = (tn,st'):ths}
        Just _ -> EError $ "Variable " ++ v1 ++ " not bound to record"

exec (NewName v1) e s@State{sas = sas, newname = n} =
  case check_vars e [v1] of
    Left msg -> EError msg
    Right [id1] -> 
      case slookup id1 sas of
        Nothing -> EError $ "Impossible: storev missing id " ++ show id1
        Just (id:vg,SUnbound) ->
          ENext s{sas = supdate sas id (SName n), newname = n+1}
        Just _ -> EError $ "NewName variable " ++ v1 ++ " previously bound"

exec (Apply v1 vs) e s@State{front = (tn,st):ths, sas = sas} =
  case check_vars e (v1:vs) of
    Left msg -> EError msg
    Right (id1:ids) -> 
      case slookup id1 sas of
        Nothing -> EError $ "Impossible: store missing id " ++ show id1
        Just (_,SUnbound) -> ESuspend
        Just (_,SThunk id) -> ENeed id id1
        Just (_,SProc (args,body,ce)) ->
          if length args /= length vs then EError "Wrong number of arguments"
          else let e' = zip args ids ++ ce
                   st' = map (tplSec e') body ++ st
               in ENext s{front = (tn,st'):ths}
        Just (_,SPrimOp f) -> f s ids
        Just _ -> EError $ "Variable " ++ v1 ++ " not bound to procedure"

exec (IsDet v1 v2) e s@State{sas = sas} =
  case check_vars e [v1,v2] of
    Left msg -> EError msg
    Right [id1,id2] -> 
      case slookup id1 sas of
        Nothing -> EError $ "Impossible: store missing id " ++ show id1
        Just (_,SUnbound) -> set_to id2 False
        Just _            -> set_to id2 True
  where set_to :: SLoc -> Bool -> ExecResult
        set_to id b =
          let bool = if b then "true" else "false"
              brec = SRec (bool, [])
          in case slookup id sas of
            Nothing -> EError $ "Impossible: store missing id " ++ show id
            Just (_,SUnbound) ->
              ENext s{sas = supdate sas id brec}
            Just (_,SRec (l,fvs)) ->
              if l == bool && null fvs then ENext s
              else EError "IsDet return value bound to wrong value"
            Just _ -> EError "IsDet return value bound to wrong value"

exec (NewCell v1 v2) e s@State{sas = sas, muts = muts, newname = n} =
  case check_vars e [v1,v2] of
    Left msg -> EError msg
    Right [id1,id2] -> 
      case slookup id2 sas of
        Nothing -> EError $ "Impossible: store missing id " ++ show id1
        Just (id:vg,SUnbound) ->
          ENext s{sas = supdate sas id (SCell n),
                  muts = (n,id1):muts, newname = n+1}
        Just _ -> EError $ "NewCell variable " ++ v2 ++ " previously bound"
  
exec (Exchange v1 v2 v3) e s@State{sas = sas, muts = muts} =
  case check_vars e [v1,v2,v3] of
    Left msg -> EError msg
    Right [id1,id2,id3] -> 
      case slookup id1 sas of
        Nothing -> EError $ "Impossible: store missing id " ++ show id1
        Just (_,SUnbound) -> ESuspend
        Just (_,SThunk id) -> ENeed id id1
        Just (_,SCell n) ->
          case lookup n muts of
            Nothing -> EError $ "Impossible: mutables missing name " ++ show n
            Just id ->
              let muts' = mupdate muts n id3
              in case bind sas id2 id of
                   Nothing ->
                     EError $ "Can't unify " ++ v3 ++ " and " ++ "@" ++ v1
                   Just sas' -> ENext s{sas = sas', muts = muts'} --UPDATED
        Just _ -> EError $ "Variable " ++ v1 ++ " not bound to a cell"
                                       
exec (ByNeed v1 v2) e s@State{sas = sas} =
  case check_vars e [v1,v2] of
    Left msg -> EError msg
    Right [id1,id2] -> 
      case slookup id2 sas of
        Nothing -> EError $ "Impossible: store missing id " ++ show id1
        Just (_,SUnbound) ->
          ENext s{sas = supdate sas id2 (SThunk id1)}
        Just _ -> ENeed id1 id2

exec (TryCatch [] v1 ms2) e s = ENext s    -- Unused handler
exec (TryCatch ms1 v1 ms2) e s@State{front = (tn,st):ths} =
  let st' = map (tplSec e) ms1 ++ (TryCatch [] v1 ms2,e) : st
  in ENext s{front = (tn,st'):ths}

exec (Raise v1) e s@State{front = (tn,st):ths, sas = sas} = unwind st where
  unwind :: Stack -> ExecResult
  unwind [] = EError "Uncaught exception"
  unwind ((TryCatch [] v2 ms2, e2):st) =
    case check_vars e [v1] of
      Left msg -> EError msg
      Right [id1] -> 
        let e' = (v2, id1) : e2
            st' = map (tplSec e') ms2 ++ st
        in ENext s{front = (tn,st'):ths}
  unwind (_:st) = unwind st


-- Schedule threads for complete execution: sched q n p s
-- q is the execution quantum (Just k: k steps, None: infinity)
-- n is the current step (1 <= n <= k)
-- p is the progress indicator (True: progress has been made, False: not yet)
-- s is the current state

data ExtInt = Finite Int | Infinity        -- Extended integers (for quantum)

data SchedResult = SDone                   -- Program terminates successfully
                 | SError String           -- Error with message

sched :: ExtInt -> Int -> Bool -> State -> SchedResult

-- Program completely done
sched q n p State{front = [], back = []} = SDone

-- Current pass through threads done
sched q n p s@State{front = [], back = back}
  | p = sched q n False s{front = reverse back, back = []}
  | otherwise = SError $ "Deadlocked Statements:"++ "\n" ++ concat (deadlockedStmts back)
    where
      deadlockedStmts [] = []
      deadlockedStmts ((n,((s,e):stck)):bck) = [show s] ++ ["\n"] ++ deadlockedStmts bck

-- Current thread done
sched q n p s@State{front = (tn,[]):ths} = sched q 1 p s{front = ths} -- FIXED FROM N

-- Current thread times out
sched (Finite k) n p s@State{front = (tn,st):ths, back = back}
  | n > k = sched (Finite k) 1 p s{front = ths, back = (tn,st):back}

-- Take one step of execution
sched q n p s@State{front = (tn,st@((m,e):st')):ths, back = back, sas = sas,
                    newthread = newt}
  = case exec m e s'' of
      EError msg -> SError msg                                     -- Error
      ESuspend -> sched q 1 p s{front = ths, back = (tn,st):back}  -- Suspension
      ENeed id id2 -> -- trace ("Needed: "++show id2) $                                                 -- Thunk
        case slookup id sas of
          Nothing -> SError $ "Impossible: store missing id " ++ show id
          Just (_, SProc ([v1],body,ce)) ->
            let e' = (v1,id2) : ce
            in sched q 1 p s{front = (newt, map (tplSec e') body):ths,
                             back = (tn,st):back, newthread = newt+1,
                             sas = supdate sas id2 SUnbound}
          Just _ -> SError $ "Thunk not a procedure of one variable"
      ENext s' -> sched q (n+1) True s'                             -- Progress
    where
      s'' = s{front = (tn,st'):ths}                           

oz ss = sched Infinity 1 False State{front = [(1,map (\x -> (x,e)) ss)], back = [], sas = s, muts = [], newlocs = [8..], newname = 1, newthread = 2}
  where 
    e = [("IntPlus",1),("IntMinus",2),("Eq",3),("GT",4),("LT",5),("Mod",6),("IntMultiply",7)]
    s = [([1],SPrimOp int_plus),([2],SPrimOp int_minus),([3],SPrimOp equality_check),([4],SPrimOp intGT),([5],SPrimOp intLT),([6],SPrimOp intMod),([7],SPrimOp int_multiply)] 


toz q ss = sched q 1 False State{front = [(1,map (\x -> (x,e)) ss)], back = [], sas = s, muts = [], newlocs = [8..], newname = 1, newthread = 2}
  where 
    e = [("IntPlus",1),("IntMinus",2),("Eq",3),("GT",4),("LT",5),("Mod",6),("IntMultiply",7)]
    s = [([1],SPrimOp int_plus),([2],SPrimOp int_minus),([3],SPrimOp equality_check),([4],SPrimOp intGT),([5],SPrimOp intLT),([6],SPrimOp intMod),([7],SPrimOp int_multiply)]