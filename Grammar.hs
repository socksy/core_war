module Grammar where

import Parsing
import qualified Redcode as RC

{-
-----------------
Program ::= Statement (Program | Empty)
Statement ::= Comment | Instruction
Comment ::= ; Line
Line ::= "\n"
Instruction ::= Command Field Field
Command ::= DAT | MOV | NOP
           | ADD | SUB | MUL | DIV
           | MOD | JMP | JMZ | JMN
           | DJN | SEQ | SNE | SLT
Field ::= Immediate | IndirectA | IndirectB | Address
Address ::= Int
Immediate ::= # Int
IndirectA ::= $ Int
IndirectB ::= @ Int
-----------------
-}

eval :: String -> [([RC.Loc], String)]
eval str = parse program str

program :: Parser [RC.Loc]
program = do x <- statement
             do space
                xs <- program
                return (x:xs)
              ||| return [x]


command :: RC.Instr -> String -> Parser RC.Loc
command inst str = do x <- string str
                      space
                      f <- field
                      char ','
                      space
                      f' <- field
                      return (RC.Loc inst f f')


statement :: Parser RC.Loc
statement = parseCommands ops

parseCommands = foldr1 (|||) . map (\(str, inst) -> command inst str)

--Big fuck off list of opcode commands
ops = [("DAT", RC.DAT), ("MOV", RC.MOV), ("NOP", RC.NOP), ("ADD", RC.ADD), ("SUB", RC.SUB), 
        ("MUL", RC.MUL), ("DIV", RC.DIV), ("MOD", RC.MOD), ("JMP", RC.JMP), ("JMZ", RC.JMZ),
        ("JMN", RC.JMN), ("DJN", RC.DJN), ("SEQ", RC.SEQ), ("SNE", RC.SNE), ("SLT", RC.SLT)]
field :: Parser RC.Field
field = immediate ||| indirectA ||| indirectB ||| address

address = do x <- Parsing.int
             return (RC.Addr x)

immediate = do char '#'
               x <- Parsing.int
               return (RC.Val x)

indirectA = do char '$'
               x <- Parsing.int
               return (RC.AInd x)

indirectB = do char '@'
               x <- Parsing.int
               return (RC.BInd x)
