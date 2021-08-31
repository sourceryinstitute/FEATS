Build FEATS
===========

This directory demonstrates the consruction of a
directed acyclic graph (DAG) in which each vertex
in the DAG is a compiling or linking step involved 
building FEATS and each edge represents a module
dependency.

The `compile_or_link_{m,s}.f90` source files inside
[../src/example-lib] are intended to represent user
code and therefore belong in the current subdirectory, 
but placing them here causes a linking error with the
Fortran Package Manager (fpm).

[../src/example-lib]: ../src/example-lib
