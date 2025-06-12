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
alr gnatprove --mode=all --prover=all --level=2
```

Speed up evaluation using parallel analysis with the `-j` flag, like `-j12` if
you have 12 logical cores on your machine.
