{-# LANGUAGE ConstraintKinds, Rank2Types, GADTs, GADTSyntax #-}
module TM where
    import Prelude
    import qualified Data.Map.Strict as Map
    import Debug.Trace
    
    -- States of a Turing Machine, other than the starting state and the halting 
    -- state, are identified with labels. Labels are also used to identify 
    -- symbols.
    type Label a    = (Show a, Eq a, Ord a)
    
    -- The data that can be stored in the cells of the tape of a Turing 
    -- Machine, are Symbols. The special symbol Blank denotes an empty cell
    -- under the tape head.
    data Symbol b where
        Blank   :: Symbol b
        Symbol  :: (Label b) => b -> Symbol b
        
    instance Show (Symbol b) where
        show Blank      = "_"
        show (Symbol s) = (show s)
        showList ls s   = 
            let showList' _ [] s = s
                showList' showx (x:xs) s    = showx x (showl xs) where   
                    showl []                = s
                    showl (y:ys)            = showx y (showl ys)
            in showList' shows ls s
        
    instance Eq (Symbol b) where
        s == t = 
            case (s,t) of
                (Blank, Blank)          -> True
                (Symbol x, Symbol y)    -> x == y
                (_, _)                   -> False
                    
    instance Ord (Symbol b) where
        compare s t = 
            case (s,t) of
                (Blank, Blank)          -> EQ
                (Blank, _)              -> LT
                (_, Blank)              -> GT
                (Symbol x, Symbol y)    -> compare x y
    
    -- A Turing Machine has a special starting state and a halting state. 
    -- Additionally, it may have other states identified with labels.
    data State a where
        Start   :: State a
        Halt    :: State a
        State   :: Label a => a -> State a
        
    instance Show (State a) where
        show x = case x of
            Start   -> "state S"
            Halt    -> "state H"
            State y -> "state " ++ (show y)
        
    instance Eq (State a) where
        Start       == Start        = True
        Halt        == Halt         = True
        (State x)   == (State y)    = x == y
        _           == _            = False
        
    instance Ord (State a) where
        compare Start Start         = EQ
        compare Halt Halt           = EQ
        compare Start _             = LT
        compare Halt _              = GT
        compare _ Start             = GT
        compare _ Halt              = LT
        compare (State x) (State y) = compare x y
        
    -- The tape of the Turing Machine is represented as two lists. The left list 
    -- contains the part of the tape to the left of the tape head while the 
    -- right part contains the rest.
    type Tape b     = ([Symbol b], [Symbol b])
        
    -- The configuration of a Turing Machine contains the current state, 
    -- position of the tape head, and the contents of the tape.
    data Configuration a b where 
        Cfg :: (Label a, Label b) => {
            state   :: State a, 
            tape    :: Tape b
        } -> Configuration a b
        
    instance Show (Configuration a b) where
        show (Cfg s (ls, (rs))) =
            let show' []        = "\x1B[1m_\x1B[0m"
                show' (x:xs)    = "\x1B[1m" ++ (show x) ++ "\x1B[0m" ++ (show xs)
            in  (show s) ++ ";\t" ++ (show (reverse ls)) ++ (show' rs)
        
    -- The state transition of a Turing Machine contains a direction to which
    -- the tape head moves. L, I, and R stand for left, idle, and right 
    -- respectively.
    data Direction = L | I | R deriving Show

    -- The transition table specifies the behaviour of a Turing Machine in terms
    -- of the current state and symbol under the tape head. The action taken by
    -- the machine depends on the mapped value.
    type TransitionTable a b = (Label a, Label b) => 
        Map.Map (State a, Symbol b) (Symbol b, Direction, State a)
    -- TODO: Transitions from the halting state should be prevented somehow.
    
    -- Produces a very human-readable description of the instruction set of a
    -- Turing Machine.
    fancyShow :: (Label a, Label b) => TransitionTable a b -> String
    fancyShow tt    = 
        let show'' d = case d of
                    L   -> "move the tape head to the left, "
                    R   -> "move the tape head to the right, "
                    I   -> ""
            show' ((q, s), (s', d, q')) = "At " ++ (show q) ++ " with " ++ 
                (show s) ++ " on the tape head, write " ++ (show s') ++ ", " ++ 
                (show'' d) ++ "and go to the " ++ (show q') ++ "."
            fancyShow' [] _     = "    The end."
            fancyShow' (x:xs) n = "    " ++ (show n) ++ ".\t" ++ (show' x) ++ 
                "\n" ++ (fancyShow' xs (n+1))
        in "Instruction Set:\n" ++ (fancyShow' (Map.assocs tt) 1)
        
    -- A Turing Machine is determined by the its transition table and initial
    -- configuration.
    data TuringMachine a b where
        TM :: (Label a, Label b) => {
            tt  :: TransitionTable a b,
            cfg :: Configuration a b
        } -> TuringMachine a b 
        
    instance Show (TuringMachine a b) where
        show (TM tt cfg) = (fancyShow tt) ++ "\nInitial Configuration:\n    " ++ 
            (show cfg)
        
    -- A helper function for creating a Turing Machine. Transforms a transition
    -- table and a tape into a Turing Machine with the initial configuration
    -- being the starting state and the given tape.
    compile :: (Label a, Label b) => 
        TransitionTable a b -> Tape b -> TuringMachine a b
    compile tt t = TM tt (Cfg Start t)

    -- A helper function that simulates the movement of the tape head to the 
    -- right.
    right :: Tape b -> Tape b
    right (xs, [])  = (xs, [])
    right (xs, y:ys)= (y:xs, ys)

    -- A helper function that simulates the movement of the tape head to the 
    -- left.
    left :: Tape b -> Tape b
    left ([], ys)   = ([], ys)
    left (x:xs, ys) = (xs, x:ys)
    
    -- A helper function that returns the current symbol under the tape head.
    current :: Tape b -> Symbol b
    current (_,[])  = Blank
    current (_,x:xs)= x

    -- A helper function that simulates the action of replacing the symbol under
    -- the tape head and possibly moving it.
    write :: Tape b -> Symbol b -> Direction -> Tape b
    write (ls, rs) s d = 
        let move = case d of
                L       -> left
                R       -> right
                I       -> id       :: Tape b -> Tape b
            replace xs y = case xs of 
                (x:xs)  -> y:xs
                _       -> [y]
        in  move (ls, replace rs s)
        
    -- Performs a single step of computation on the given Turing Machine.
    step :: TuringMachine a b -> TuringMachine a b
    step tm@(TM tt (Cfg q t)) =
        case (tt Map.!? (q, current t)) of
                Just (s, d, q') -> TM tt (Cfg q' (write t s d))
                Nothing         -> tm
        
    -- Constructs and runs a Turing Machine with the given input until it halts 
    -- or the given maximum number of steps is exceeded.
    run :: (Label a, Label b) => (c -> TuringMachine a b) -> c -> Int -> IO ()
    run f x n =
        let tm = f x
            run' tm@(TM _ cfg@(Cfg q _)) k
                | q == Halt || k == n   = 
                    trace "Final Configuration:" $ ("    ") ++ (show cfg)
                | otherwise = 
                    trace   ("    Step " ++ (show k) ++ ":\t" ++ (show cfg)) 
                            (run' (step tm) (k + 1))
        in  putStrLn ((show tm) ++ "\nSequence:\n" ++ (run' tm 0))
        
    -- Multiplies the given integer by two. Non-positive inputs are interpreted 
    -- as zero. The format of this Turing Machine follows the definition given
    -- in Nigel Cutland's "Computability: An Introduction to Recursive Function 
    -- Theory" translated into the TMS model.
    -- TODO: Use Maybe to properly handle negative inputs.
    unaryMultiplier :: Int -> TuringMachine Int Int
    unaryMultiplier n = compile is
        (take (k * 2 + 1) $ cycle [Blank], take (k + 1) $ cycle $ [Symbol 1])
        where   k = if (n >= 0) then n else 0
                is = Map.fromList 
                    [
                        ((Start,   Symbol 1 ), (Blank,      I, State 1)),
                        ((State 1, Blank    ), (Blank,      R, State 2)),
                        ((State 2, Blank    ), (Blank,      I, Halt   )),
                        ((State 2, Symbol 1 ), (Symbol 1,   R, State 3)),
                        ((State 3, Symbol 1 ), (Symbol 1,   R, State 3)),
                        ((State 3, Blank    ), (Blank,      L, State 4)),
                        ((State 4, Symbol 1 ), (Blank,      I, State 5)),
                        ((State 5, Blank    ), (Blank,      L, State 6)),
                        ((State 6, Symbol 1 ), (Symbol 1,   L, State 6)),
                        ((State 6, Blank    ), (Blank,      L, State 7)),
                        ((State 7, Symbol 1 ), (Symbol 1,   L, State 7)),
                        ((State 7, Blank    ), (Symbol 1,   I, State 8)),
                        ((State 8, Symbol 1 ), (Symbol 1,   L, State 8)),
                        ((State 8, Blank    ), (Symbol 1,   I, State 9)),
                        ((State 9, Symbol 1 ), (Symbol 1,   R, State 9)),       
                        ((State 9, Blank    ), (Blank,      R, State 2))
                    ] 
