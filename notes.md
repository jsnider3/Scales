## Compiler notes

The JVM is a hardcore stack machine. All variables are on an imaginary
stack and all instructions get their args from that stack unless they take
params explicitly.

* `ldc` - is the jasmin instruction for constants.
* `newarray int` - is the instruction for making arrays.
* `getfield` and `putfield` - are for reading/writing class fields.
* `new` - does what `new` does.
* `goto` and `if*` - are branches and loops.
* `invokevirtual` and `invokenonvirtual` - are for methods.


### Stack inconsistency

The Inconsistent stack height error is caused by different branches,
pushing/popping different amounts. `compile` should probably have an `Int`
as its return type which can be used by ifs/whiles to add
necessary pops/dups. Since we can't take back printing. We can insert
pops/dups at the end of the else cases in if statements and we should only
need to add pops at the end of while bodies. This problem is now solved.

### Variable limits.
There's a limit to how many variables can be stored in "pseudo-registers",
this will also be a major annoyance.
