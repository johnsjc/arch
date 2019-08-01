### MIPS

Projects to learn MIPS.
* fibonacci - implementations of algorithms to determine the nth fibonacci number (recursive with memoization, tail recursive, and iterative)
* sort - implementations of bubble sort and recursive and iterative merge sort algorithms to sort a list of arbitrary size in ascending order.
---
[Reference Card](http://www.cburch.com/cs/330/reading/mips-ref.pdf)

#### About MIPS

RISC architecture where each instruction is 32-bits (4 bytes).

Bitfield:
```
+-----------+--------+--------+--------+--------+--------+--------+
|           | 6 bits | 5 bits | 5 bits | 5 bits | 5 bits | 6 bits |
+-----------+--------+--------+--------+--------+--------+--------+
| Register  | op     | reg1   | reg2   | dest   | shift  | func   |
+-----------+--------+--------+--------+--------+--------+--------+
| Immediate | op     | reg1   | reg2   |     16-bit constant      |
+-----------+--------+--------+--------+--------------------------+
| Jump      | op     |             26-bit constant                |
+-----------+--------+--------------------------------------------+
```
The first six bits of each instruction are the `op` field.
* Determines if the instruction is a `register` `immediate` or `jump` instruction.
  * Determines how the rest of the bits are interpreted.
* If the op field is 0, then the instruction is a `register` instruction.
* e.g. 0x02918020 is the `add, $s0, $s4, $s1` operation.
  * 0x2918020 = 0000 0010 1001 0001 1000 0000 0010 0000
  * Apply bit field: 000000 10100 10001 10000 00000 100000
  * First 6 bits = 000000 `register` instruction
  * `reg1` = 10100 which is `r20` or `$s4`
  * `reg2` = 10001 which is `r17` or `$s1`
  * `destination` = 10000 which is `r16` or `$s0`
  * `shift` = 00000
  * `func` = 100000 (`add` operation)  

#### Calling Convention
* The caller must do the following:
    * Arguments go in the `$a0-$a3` registers. Additional arguments are pushed on the stack.
    * Save any **caller-save registers** (`$t0-$t9`) which are used by the caller.
    * Pass control to the called function with a `jal` or `jalr` instruction.
    * Restore the caller-save registers after the call.
    * Pop any arguments that were pushed on the stack.
    * Extract the return value(s) from `$v0` and `$v1`.
* The callee must do the following:-
    * Create a stack frame 
        * Subtract the frame size from the stack pointer `$sp`.
    * Save any **callee-save registers** (`$s0-$s7`, `$fp`, and `$ra`) that are used by the function.
        * The return address `$ra` only needs to be saved if the function will call another function.
    * Put the return value(s) in `$v0` and `$v1`.
    * Restore the callee-save registers before returning.
    * Resume execution at before the function call with `jr $ra`.
