module Redcode where

type PC = Int -- Program counter

data Field = Val  { field_val :: Int }
           | Addr { field_val :: Int }
           | AInd { field_val :: Int }
           | BInd { field_val :: Int }

instance Show Field where
    show (Val i) = "#" ++ show i
    show (Addr i) = show i
    show (AInd i) = "$" ++ show i
    show (BInd i) = "@" ++ show i

data Instr = DAT | MOV | NOP
           | ADD | SUB | MUL | DIV
           | MOD | JMP | JMZ | JMN
           | DJN | SEQ | SNE | SLT
  deriving (Eq,Show)

data Loc = Loc { instruction :: Instr,
                 a_field :: Field,
                 b_field :: Field 
               }

instance Show Loc where
    show (Loc l x y) = show l ++ " " ++ show x ++ " " ++ show y

type Core = [Loc]

data Warrior = Warrior { warrior_name :: String,
                         warrior_code :: [Loc] }

coresize = 1024

init_core :: Core
init_core = take coresize (repeat (Loc DAT (Val 0) (Val 0)))


data System = S { system_core :: [Loc],
                  processes :: [(String, PC)] }

instance Show System where
    show (S core pcs) = sc' 0 core
      where
        sc' i (x : xs) = show i ++ ":\t" ++ show x ++ 
                         showPC i pcs ++ "\n" ++
                         sc' (i+1) xs
        sc' i [] = ""
        showPC i [] = ""
        showPC i ((p,i'):xs) | i == i' = "\t <- " ++ p
                             | otherwise = showPC i xs
                         

init_state = S init_core []

-- Update the value at the given index in a list

updateIndex :: Int -> a -> [a] -> [a]
updateIndex 0 x (_:ys) = x:ys
updateIndex n x (y:ys) = y : updateIndex (n-1) x ys

-- Add a warrior to the core at a specific location, and set its program 
-- counter

addWarrior :: Int -> Warrior -> System -> System
addWarrior l (Warrior name w) (S core pcs) 
    = S (add' l w core) ((name, l) : pcs)
  where
    add' l (w:ws) core = add' (l+1) ws (updateIndex l w core)
    add' l []     core = core
 
imp, dwarf :: Warrior
imp = Warrior "Imp" [Loc MOV (Addr 0) (Addr 1)]

dwarf = Warrior "Dwarf"
           [Loc ADD (Val 4)  (Addr 3),
            Loc MOV (Addr 2) (BInd 2),
            Loc JMP (Val (-2)) (Addr 0),
            Loc DAT (Val 0) (Val 4)]

test_state = addWarrior 5 dwarf (addWarrior 30 imp init_state)

