module Assignment2 where

import Prelude hiding (Enum(..), sum)
import Data.List
--TASK 1--

data Cmd  = Pen Mode
          | Move ( Expr , Expr )
          | Define Macro [String] Prog
          | Call Macro [Expr]



data Expr = S Lets 
          | I Bers
          | Add Expr Expr

type Lets = String
type Bers = Int
type Macro = String
data Mode = Down | Up
type Prog = [Cmd]
--Task 2--
line = Define "line" ["x1", "y1", "x2", "y2"] [ Move (S "x1", S "y1"), Pen Down, Move (S "x2",S "y2"), Pen Up]  

--Task 3--
nix = Define "nix" ["x","y","w", "h"] [ Call "line" [S "x",S "y",S "w",S "y"],  Call "line" [S "w", S "y", S "x", S "h"] ]

--Task 4--
steps :: Int -> Prog
steps 0 = []
steps x = [ Pen Down, Move (I (x-1), I x), Move (I x, I x), Pen Up ]

--Task 5--
macros :: Prog -> [Macro]
macros [] = []
macros (x:xs) = if boolMacro x == True then getMacro x : macros xs else macros xs

boolMacro :: Cmd -> Bool
boolMacro (Define x y z) = True

getMacro :: Cmd -> Macro
getMacro (Define x y z) = x

--Task 6--
--Pretty sure I got this almost all of the way there. I was getting error messages for the "Move" having too many 
--arguments, and I couldn't figure out how to format the printexpr to y with map.. 
pretty :: Prog -> String
pretty [] = []
pretty (x:xs) = printCmds x ++ pretty xs

printCmds :: Cmd -> String
printCmds (Define x y z) = "Define " ++ x ++ ": " ++ (printstring y) ++ " " ++( pretty z ) ++ "\n"
--printCmds (Call x y) = "Call" ++ " " ++ x ++ " " ++ (map printexpr y) ++ "\n"
--printCmds (Move x y) = "move: " ++ printexpr x ++ " " ++ printexpr y ++"\n"
printCmds (Pen Up) = "Pen Up\n"
printCmds (Pen Down) = "Pen Down\n"

printstring :: [String] -> String
printstring [] = []
printstring [x] = x
printstring (x:xs) = x ++ "," ++ printstring xs

printexpr :: Expr -> String
printexpr (I x) = show x
printexpr (S x) = x
printexpr (Add x y) = "Add " ++ printexpr x ++ printexpr y