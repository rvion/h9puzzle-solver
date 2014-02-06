module Tool.Puzzle.Solver.Internal
    (
    ) where
{----------------------------------------

        We need to solve a puzzle.
        There is 9 small square pieces
        
        We need to place them in 
        order to make a 3*3 square
        
        Each piece have 4 motifs: one 
        per side the 4 motifs are taken 
        from the following list :
        
        bu && bd
        ru && rd
        pu && pd
        gu && gd
        
        Each motif needs
        to be place against its 
        complementary motif.
        
-----------------------------------------}
-- $ hi --package-name "h9puzzle-solver" --module-name "Tool.Puzzle.solver" --author "Rémi Vion" --email "vion.remi@gmail.com"

{----------------------------------------
                IMPORTS
-----------------------------------------}

import qualified Data.List as L

type Carte = (Int, Int, Int, Int)

nul :: Carte
nul = (0,0,0,0)

bu =  1 :: Int
bd = -1 :: Int
ru =  2 :: Int
rd = -2 :: Int
pu =  4 :: Int
pd = -4 :: Int
gu =  8 :: Int
gd = -8 :: Int


-- Test whenever a combinaison is correct or not
testp :: Carte -> Carte -> Carte -> [Carte] -> Int -> Bool
testp pr1 pr2 pr3 [] _ = True
testp pr1 pr2 pr3 ((a,b,c,d):xs) carte =
  (   (if complement a b then (testp (a,b,c,d) pr1 pr2 xs (carte+1)) else False)
   || (if complement b c then (testp (b,c,d,a) pr1 pr2 xs (carte+1)) else False)
   || (if complement c d then (testp (c,d,a,b) pr1 pr2 xs (carte+1)) else False)
   || (if complement d a then (testp (d,a,b,c) pr1 pr2 xs (carte+1)) else False) )
  where 
  complement motif1 motif2 =   
    if (  (if ( L.elem carte [0,3,6]) then True else (corgauche motif1 pr1))
       && (if ( L.elem carte [0,1,2]) then True else (corhaut   motif2 pr3)) ) then True else False
  
  -- special test for corners
  corgauche :: Int -> Carte -> Bool
  corgauche a (b1, b2, b3, b4) = ( a + b3 == 0 )

  corhaut :: Int -> Carte -> Bool
  corhaut a (b1, b2, b3, b4) = ( a + b4 == 0 )

--Sum of 4-uplets
somme :: Carte -> Int
somme (a1, a2, a3, a4) = a1+ a2+ a3+ a4

p = L.permutations


exempleSet =               -- List of differents cards I have, this is where the problem is set up
  [( bu, pu, bd, gd ) -- 1
  ,( bu, gu, bd, pd ) -- 2
  ,( ru, bu, pd, gd ) -- 3
  ,( ru, gu, bd, rd ) -- 4
  ,( ru, pu, gd, bd ) -- 5
  ,( bu, gu, rd, pd ) -- 6
  ,( bu, gu, pd, rd ) -- 7
  ,( ru, gu, bd, pd ) -- 8
  ,( bu, pu, rd, gd )]-- 9 
  
-- Print the solution, MAIN
main = solve exempleSet
solve cardSet = print [x | x <- L.permutations cardSet, testp nul nul nul x 0]
