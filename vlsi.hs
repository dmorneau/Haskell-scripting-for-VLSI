import Text.StringTemplate
import Data.List (find)

type Node = String    -- eg. "`A1" or "`STATE0"
type Signal = String  -- eg. "00x" or "10001"
type Transition = (Node, Node, Signal) -- Start, end, signal

-- Description of the transition graph
patterns = [(a0, a1, "x0x") -- State 0
           ,(a0, a2, "x10")
           ,(a0, a0, "x11")
           ,(a1, a0, "0x1") -- State 1
           ,(a1, a2, "xx0")
           ,(a1, a1, "1x1")
           ,(a2, a0, "0xx") -- State 2
           ,(a2, a1, "10x")
           ,(a2, a2, "11x")]
    where (a0, a1, a2) = ("`BGRNT0", "`BGRNT1", "`BGRNT2")

-- Main function, generates the tests (VHDL code) for the specified patterns and writes to a file
main = writeFile "generatedtests.txt" 
       $ "\n //BEGIN GENERATED CODE\n" ++ testPatterns patterns ++ "\n //END OF GENERATED CODE\n"

-- Takes a list of desired patterns and generates the test
testPatterns :: [Transition] -> String
testPatterns patterns = pairs pp >>= (uncurry . interpolate $ pp) >>= testTransition
    where pp = patterns >>= readpath

-- Separates paths which have "don't care" signals.
-- eg. 00x will become 001, 000
readpath :: Transition -> [Transition]
readpath (x, y, []) = [(x, y, [])]
readpath (x, y, ('x':ps)) = map (addc3 '0') subpaths ++ map (addc3 '1') subpaths
    where subpaths = readpath (x, y, ps)
readpath (x, y, (p:ps)) = map (addc3 p) $ readpath (x, y, ps)
addc3 c (x, y, p) = (x, y, c:p)

-- Adds a return path when needed.
-- eg. interpolating (a, b, _) and (c, e, _) will add a path (as found in the link list)
-- between b and c so that the test may continue.
interpolate :: [Transition] -> Transition -> Transition -> [Transition]
interpolate trans t1@(x1, y1, p1) t2@(x2, y2, p2) = 
    if y1 == x2 then [t1]
    else [t1, ret] 
        where ret = case find (\(x,y,_) -> x == y1 && y == x2) trans of
                      Just path -> path
                      Nothing -> error "Dead end in graph"

-- Generates a list of all transitions between two elements
-- eg. pairs "abc" will return [('a', 'b'), ('b', 'c')]
pairs :: [a] -> [(a,a)]
pairs xs = zipWith (,) xs (tail xs)

-- Writes the Verilog code to test a given transition.
testTransition :: Transition -> String
testTransition (_, next, signal) = render 
                                   . setAttribute "input" signal
                                   . setAttribute "next" next 
                                   $ testTemplate

-- $input$ Signal to apply on the BREQ bus
-- $next$  Expected next state
testTemplate :: StringTemplate String
testTemplate = newSTMP . unlines $ 
       [""
       ,"breq_ <= 3'b$input$;"
       ,"\\$display( \"Current state (BGRNT) is: %b. Inputting: %b\", bgrnt_, breq_);"
       ,"#(STEP * 1)"
       ,"if ( bgrnt_ == $next$ ) begin"
       ,"   \\$display( \"Transitioned to: %b ($next$) OK\", bgrnt_);"
       ,"end else begin"
       ,"   \\$display( \"Transitioned to: %b FAILURE, expected %b ($next$)\", bgrnt_, $next$);"
       ,"   \\$finish();"
       ,"end\n"]
