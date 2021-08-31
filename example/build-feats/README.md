Build FEATS
===========

This directory demonstrates the consruction of a
directed acyclic graph (DAG) in which each vertex
in the DAG is a compiling or linking step involved
building FEATS and each edge represents a module
dependency.

Building and executing
----------------------
With your present working directory set to the
top-level FEATS directory, execute
```
fpm run --example build-feats
```
which should download the FEATS dependencies if necessary, 
(re)build the dependencies and FEATS if necessary, 
execute the build-feats example, and produce the following
output:
```
 ----- application_generator(): module_depenencies DAG written to feats-dependencies.pdf
 ----- application_generator(): application defined
```

The `compile_or_link_{m,s}.f90` source files inside
[../src/example-lib] are intended to represent user
code and therefore belong in the current subdirectory, 
but placing them here causes a linking error with the
Fortran Package Manager (fpm).

[../src/example-lib]: ../src/example-lib
