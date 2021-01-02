# GPECN1DTDPBC

GPECN1DTDPBC is a Fortran-based program that solves the time-dependent
Gross-Pitevsekii equation (GPE) in a uniformly rotating reference frame
for a single component Bose-Einstein condensate (BEC) in one-dimension
using the Crank-Nicolson finite difference scheme. 

This implementation of the Crank-Nicolson scheme attempts to properly 
account for the time-dependence of the external and mean-field
potentials of the condensate by deriving the scheme using the integral
form of the unitary time-evolution operator for a time-dependent 
Hamiltonian. An iterative approach is then used to determine the unknown
mean-field potential at each time step. 

In addition, the program assumes an S1 configuration space for the 
system (i.e., the condesate is propagating in a one-dimensional, cicular
ring) and therefore implements periodic boundary conditions using the 
Sherman-Morrison algorithm.

## USAGE

make
./gpecn1Dtdpbc.sh

## DEPENDENCIES

GPECN1DTDPBC depends on the ZGTSV and XERBLA subroutines from LAPACK 
version 3.1.0. The source code for these subroutines as well as the 
LAPACK LICENSE file are distributed with GPECN1DTDPBC and may be
found in libraries/lapack subdirectory.

## CITATION

To cite the use of this work in a scientific publication, please use the
following reference:

```
@unpublished{kandesmc:2011a,
    author = "Kandes, M. C.", 
    title = "Sagnac Interferometry with Bose-Einstein Condensates in
             a Uniformly Rotating Ring Trap",
    school = "Claremont Graduate University \& San Diego State 
              University",
    note = "Ph.D. Qualifying Exam",
    year = "2011",
}
```

## Author

Marty Kandes, Ph.D.  
Computational & Data Science Research Specialist  
High-Performance Computing User Services Group  
San Diego Supercomputer Center  
University of California, San Diego  

## COPYRIGHT
     
Copyright (c) 2010 - 2021 Martin Charles Kandes

## LICENSE

The MIT License (MIT)

## LAST UPDATED

Saturday, January 2nd, 2021
