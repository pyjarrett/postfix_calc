# postfix_calc

A postfix expression calculator which provably terminates, catches all
cases of numerical underflow and overflow, and obeys all stated preconditions
and postconditions.

## Execution

Run with Alire:

```
alr run
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
