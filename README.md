# postfix_calc

A postfix expression calculator which provably terminates, catches all
cases of numerical overflow, and obeys all stated preconditions
and postconditions.

## Execution

Run with Alire:

```
alr run
```

Operations:

* `+`, `-`, `*`, `/` - basic arithmetic operations
* `.` - pop and print the top element
- `negate` - negate the top element
- `dup` - duplicate the top element
- `dump` - dump the stack
- `reset` - reset any error conditions

Example:

```
OK: Stack: 0 / 1024 > 4 5 + 3 - .
 6.00000000000000E+00
```

## Running proofs

Install `gnatprove` with Alire:

```
alr with gnatprove
```

Then run the provers.

```
alr gnatprove --level=2
```

Speed up evaluation using parallel analysis with the `-j` flag, like `-j12` if
you have 12 logical cores on your machine.

## Example proof output

This is an example proof output in gnatprove.out for `alr gnatprove --level=2`
on commit ac47c1ad5aa8f8276d975e31b13425bc281f8910.

```
PS D:\dev\ada\postfix_calc> alr gnatprove -j12 --level=2
Phase 1 of 3: generation of data representation information ...
Phase 2 of 3: generation of Global contracts ...
Phase 3 of 3: flow analysis and proof ...
Summary logged in D:\dev\ada\postfix_calc\obj\development\gnatprove\gnatprove.out

=========================
Summary of SPARK analysis
=========================

-----------------------------------------------------------------------------------------------------------------------
SPARK Analysis results        Total        Flow                                          Provers   Justified   Unproved
-----------------------------------------------------------------------------------------------------------------------
Data Dependencies                18          18                                                .           .          .
Flow Dependencies                 3           3                                                .           .          .
Initialization                   13          13                                                .           .          .
Non-Aliasing                      .           .                                                .           .          .
Run-time Checks                 197           .    197 (CVC5 95%, Trivial 4%, Z3 1%, altergo 0%)           .          .
Assertions                       24           .                        24 (CVC5 98%, Trivial 2%)           .          .
Functional Contracts             84           .    84 (CVC5 74%, Trivial 17%, Z3 8%, altergo 2%)           .          .
LSP Verification                  .           .                                                .           .          .
Termination                      47          44                                         3 (CVC5)           .          .
Concurrency                       .           .                                                .           .          .
-----------------------------------------------------------------------------------------------------------------------
Total                           386    78 (20%)                                        308 (80%)           .          .


max steps used for successful proof: 5526

============================
Most difficult proved checks
============================

No check found with max time greater than 1 second

========================
Detailed analysis report
========================

Analyzed 3 units
in unit machines, 22 subprograms and packages out of 22 analyzed
  Machines at machines.ads:3 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Machines.Execute at machines.ads:91 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (17 checks)
  Machines.Is_Running at machines.ads:32 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Machines.Is_Stack_Empty at machines.ads:43 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Machines.Is_Stack_Full at machines.ads:47 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Machines.Is_Stopped at machines.ads:36 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Machines.Machine at machines.ads:116 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Machines.Op_Add at machines.ads:134 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (12 checks)
  Machines.Op_Divide at machines.ads:173 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (14 checks)
  Machines.Op_Dupe at machines.ads:198 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (9 checks)
  Machines.Op_Multiply at machines.ads:160 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (11 checks)
  Machines.Op_Negate at machines.ads:186 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (6 checks)
  Machines.Op_Print at machines.ads:214 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (2 checks)
  Machines.Op_Subtract at machines.ads:147 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (12 checks)
  Machines.Peek at machines.ads:51 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  Machines.Peek at machines.ads:54 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  Machines.Pop at machines.ads:72 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (5 checks)
  Machines.Pop at machines.ads:82 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (5 checks)
  Machines.Push at machines.ads:59 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (13 checks)
  Machines.Stack_Size at machines.ads:40 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Machines.Status at machines.ads:29 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Machines.To_Machine_Op at machines.ads:100 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
in unit postfix_calc, 0 subprograms and packages out of 0 analyzed
in unit scanners, 27 subprograms and packages out of 27 analyzed
  Scanners at scanners.ads:4 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (4 checks)
  Scanners.Contains at scanners.ads:101 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (2 checks)
  Scanners.Contains at scanners.ads:102 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Scanners.Has_Input at scanners.ads:26 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Scanners.Has_Lexeme at scanners.ads:30 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Scanners.Has_More_Characters at scanners.ads:28 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Scanners.Has_Next at scanners.ads:61 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (5 checks)
  Scanners.Has_Valid_Cursor at scanners.ads:147 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Scanners.Has_Valid_State at scanners.ads:150 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Scanners.Ignore_Lexeme at scanners.ads:32 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (9 checks)
  Scanners.Image at scanners.ads:99 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  Scanners.Input_Size at scanners.ads:45 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Scanners.Is_Number at scanners.ads:113 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (14 checks)
  Scanners.Is_Valid at scanners.ads:136 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Scanners.Is_Word at scanners.ads:103 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  Scanners.Lexeme_Range at scanners.ads:123 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Scanners.Lexeme_Size at scanners.ads:40 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (3 checks)
  Scanners.Load_Input at scanners.ads:47 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (20 checks)
  Scanners.Next at scanners.ads:70 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (15 checks)
  Scanners.Next_Token at scanners.ads:89 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (43 checks)
  Scanners.Peek at scanners.ads:68 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (2 checks)
  Scanners.Remaining_Characters at scanners.ads:43 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
  Scanners.Scanner at scanners.ads:138 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (0 checks)
  Scanners.Skip_Whitespace at scanners.ads:187 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (34 checks)
  Scanners.Take_Lexeme at scanners.ads:221 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (21 checks)
  Scanners.Tokenize at scanners.ads:105 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (24 checks)
  Scanners.Width at scanners.ads:18 flow analyzed (0 errors, 0 checks, 0 warnings and 0 pragma Assume statements) and proved (1 checks)
```