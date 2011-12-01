VLSI Verification with Haskell

==============================

Haskell is very good at compiler-like problems, such as generating
Verilog code. I've often been frustrated by Verilog's lack of power as
a programming language. This script, given a state machine's
description, generates an exhaustive series of tests, which can then
be inserted in the test bench. The generated code worked well, and I
believe its correctness to be much more obvious than for the
hand-written code that other people used.

The StringTemplate library worked very nicely for this project.

(For the curious: the particular state machine used here is a bus
arbiter.)