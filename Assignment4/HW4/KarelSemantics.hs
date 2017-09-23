-- Charles Henninger (henninch)

module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) w r= not(test t w r)
test (Facing c) _ r = if c == getFacing r then True else False 
test (Clear d) w r = isClear (relativePos d r) w
test (Beeper)   w r = hasBeeper (getPos r) w
test (Empty)    _ r = isEmpty r

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt Move _ w r = let x = relativePos Front r in if isClear x w then OK w (setPos x r) else Error ("blocked")
stmt PickBeeper _ w r = let x = getPos r in if hasBeeper x w then OK (decBeeper  x w) (incBag r) else Error ("There is no beeper there")
stmt PutBeeper _ w r = let x = getPos r in if isEmpty r then Error ("You have zero beepers") else OK (incBeeper x w) (decBag r)
stmt (Turn d) _ w r = let x = getFacing r in OK w (setFacing (cardTurn d x)  r)
stmt (Iterate 0 eval) x w r = OK w r
stmt (Iterate y eval) x w r = if y == 0 then Error ("Dear God, the loop has failed") else case stmt eval x w r of
																																			(OK w' r') -> stmt (Iterate (y - 1) eval) x w' r'
																																			(Done r') -> Done r'
																																			(Error m) -> Error m
stmt (Call y) x w r = case lookup y x of 
                                        (Just z) -> stmt z x w r 
                                        _ -> Error ("Undefined")
stmt (If x y z) v w r = if (test x w r == True)
                                                then stmt y v w r
                                                else stmt z v w r
stmt (While x y) z w r = if test x w r 
                                                 then case stmt y z w r of
                                                 (OK w' r') -> stmt (While x y) z w' r'
                                                 (Done r') -> Done r'
                                                 (Error m) -> Error m
                                            else OK w r
stmt (Block []) x w r = OK w r
stmt (Block (x:xs)) y w r = case stmt x y w r of
                                       (OK w' r') -> stmt (Block xs) y w' r'
                                       otherwise -> otherwise
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
