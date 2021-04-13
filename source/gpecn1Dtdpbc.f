! =====================================================================
! NAME
!
!    GPECN1DTDPBC - GPE = Gross-Pitaevskii Equation; 
!                   CN = Crank-Nicolson Method
!                   1D = One-Dimensional
!                   TD = Time-dependent
!                   PBC = Periodic Boundary Conditions
!
! DESCRIPTION
!
!    GPECN1DTDPBC is a Fortran-based program that solves the 
!    time-dependent Gross-Pitevsekii equation (GPE) in a uniformly 
!    rotating reference frame for a single component Bose-Einstein 
!    condensate (BEC) in one-dimension using the Crank-Nicolson finite 
!    difference scheme. 
!    
!    This implementation of the Crank-Nicolson scheme attempts to  
!    properly account for the time-dependence of the external and 
!    mean-field potentials of the condensate by deriving the scheme using
!    the integral form of the unitary time-evolution operator for a 
!    time-dependent Hamiltonian. An iterative approach is then used to  
!    determine the unknown mean-field potential at each time step. 
!
!    In addition, the program assumes an S1 configuration space for the 
!    system (i.e., the condesate is propagating in a one-dimensional, 
!    cicular ring) and therefore implements periodic boundary conditions
!    using the Sherman-Morrison algorithm.
!
! USAGE
!
!    make
!    ./gpecn1Dtdpbc.sh
!
! DEPENDENCIES
!
!    GPECN1DTDPBC depends on the ZGTSV and XERBLA subroutines from LAPACK 
!    version 3.1.0. The source code for these subroutines as well as  
!    the LAPACK LICENSE file are distributed with GPECN1DTDPBC and may be
!    found in libraries/lapack subdirectory.
!
! CITATION
!
!    To cite the use of this work in a scientific publication, please use 
!    the following reference:
!
!    @unpublished{kandesmc:2011a,
!        author = "Kandes, M. C.", 
!        title = "Sagnac Interferometry with Bose-Einstein Condensates in
!                 a Uniformly Rotating Ring Trap",
!        school = "Claremont Graduate University \& San Diego State 
!                 University",
!        note = "Ph.D. Qualifying Exam",
!        year = "2011",
!    }
!
! AUTHOR
!
!     Marty Kandes, Ph.D.
!     Computational & Data Science Research Specialist
!     User Services Group
!     San Diego Supercomputer Center
!     University of California, San Diego
!
! COPYRIGHT
!     
!     Copyright (c) 2010 - 2021 Martin Charles Kandes
!
! LICENSE
!
!     The MIT License (MIT)
!
! LAST UPDATED
!
!     Friday, March 31st, 2017                                   
!
! ----------------------------------------------------------------------
      PROGRAM GPECN1DTDPBC
      IMPLICIT NONE

!     Parameter Declarations:
!     -----------------------
      REAL, PARAMETER :: PI = 3.1415926535897932384626433832795028842E0

!     Input Variable and Array Declarations:
!     --------------------------------------
      INTEGER :: NUMBER_OF_GRID_POINTS 
      INTEGER :: NUMBER_OF_TIME_STEPS
      INTEGER :: NUMBER_OF_TIME_STEPS_BEFORE_WRITE
      INTEGER :: NUMBER_OF_ITERATIONS_PER_TIME_STEP
      INTEGER :: IMAGINARY_TIME_SWITCH

      REAL :: RADIUS
      REAL :: ROTATION_RATE
      REAL :: GAMMA_FACTOR

      COMPLEX, ALLOCATABLE, DIMENSION(:) :: WAVEFUNCTION
      COMPLEX, ALLOCATABLE, DIMENSION(:) :: EXTERNAL_POTENTIAL
      COMPLEX, ALLOCATABLE, DIMENSION(:) :: NONLINEAR_COUPLING

!     Internal Variable and Array Declarations:
!     -----------------------------------------
      CHARACTER(80) :: BUFFER

      INTEGER :: CURRENT_GRID_POINT
      INTEGER :: CURRENT_TIME_STEP
      INTEGER :: CURRENT_ITERATION
      INTEGER :: CURRENT_FILE
      INTEGER :: LENGTH
      INTEGER :: INFO

      REAL :: ANGULAR_GRID_SPACING_SIZE
      REAL :: TIME_STEP_SIZE

      REAL :: L2NORM

      COMPLEX, ALLOCATABLE, DIMENSION(:) :: TEMPORARY_STORAGE
      COMPLEX, ALLOCATABLE, DIMENSION(:) :: RIGHT_HAND_SIDE
      COMPLEX, ALLOCATABLE, DIMENSION(:) :: UPPER_DIAGONAL
      COMPLEX, ALLOCATABLE, DIMENSION(:) :: MAIN_DIAGONAL
      COMPLEX, ALLOCATABLE, DIMENSION(:) :: LOWER_DIAGONAL
      COMPLEX, ALLOCATABLE, DIMENSION(:) :: W
      COMPLEX, ALLOCATABLE, DIMENSION(:) :: Z

!     Input Variable Assignment Read from Command-line Arguments:
!     -----------------------------------------------------------
      CALL GET_COMMAND_ARGUMENT(1,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) NUMBER_OF_GRID_POINTS

      CALL GET_COMMAND_ARGUMENT(2,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) NUMBER_OF_TIME_STEPS

      CALL GET_COMMAND_ARGUMENT(3,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) NUMBER_OF_TIME_STEPS_BEFORE_WRITE

      CALL GET_COMMAND_ARGUMENT(4,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) NUMBER_OF_ITERATIONS_PER_TIME_STEP

      CALL GET_COMMAND_ARGUMENT(5,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) IMAGINARY_TIME_SWITCH

      CALL GET_COMMAND_ARGUMENT(6,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) RADIUS

      CALL GET_COMMAND_ARGUMENT(7,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) ROTATION_RATE

      CALL GET_COMMAND_ARGUMENT(8,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) GAMMA_FACTOR

!     Allocate Arrays:
!     ----------------
      ALLOCATE(WAVEFUNCTION(NUMBER_OF_GRID_POINTS))
      ALLOCATE(EXTERNAL_POTENTIAL(NUMBER_OF_GRID_POINTS))
      ALLOCATE(NONLINEAR_COUPLING(NUMBER_OF_GRID_POINTS))
      ALLOCATE(TEMPORARY_STORAGE(NUMBER_OF_GRID_POINTS))
      ALLOCATE(RIGHT_HAND_SIDE(NUMBER_OF_GRID_POINTS))
      ALLOCATE(UPPER_DIAGONAL(NUMBER_OF_GRID_POINTS-1))
      ALLOCATE(MAIN_DIAGONAL(NUMBER_OF_GRID_POINTS))
      ALLOCATE(LOWER_DIAGONAL(NUMBER_OF_GRID_POINTS-1))
      ALLOCATE(W(NUMBER_OF_GRID_POINTS))
      ALLOCATE(Z(NUMBER_OF_GRID_POINTS))

!     Internal Variable Assignment:
!     -----------------------------
      ANGULAR_GRID_SPACING_SIZE = 2.0E0*PI/FLOAT(NUMBER_OF_GRID_POINTS)
      TIME_STEP_SIZE = (RADIUS*ANGULAR_GRID_SPACING_SIZE)**2/(2.0E0*GAMMA_FACTOR)

!     Read Input Arrays from File:
!     ----------------------------
      OPEN(UNIT=1000,ACTION='READ',FORM='UNFORMATTED',STATUS='OLD')
      READ(UNIT=1000) WAVEFUNCTION
      CLOSE(UNIT=1000,STATUS='KEEP')

      OPEN(UNIT=999,ACTION='READ',FORM='UNFORMATTED',STATUS='OLD')
      READ(UNIT=999) EXTERNAL_POTENTIAL
      CLOSE(UNIT=999,STATUS='KEEP')

      OPEN(UNIT=998,ACTION='READ',FORM='UNFORMATTED',STATUS='OLD')
      READ(UNIT=998) NONLINEAR_COUPLING
      CLOSE(UNIT=998,STATUS='KEEP')

!     Initializations:
!     ----------------
      CURRENT_FILE = 1001

!     Main Program Loop:
!     ------------------
      DO CURRENT_TIME_STEP = 1, NUMBER_OF_TIME_STEPS
         TEMPORARY_STORAGE = WAVEFUNCTION
         DO CURRENT_ITERATION = 1, NUMBER_OF_ITERATIONS_PER_TIME_STEP
            UPPER_DIAGONAL = CMPLX((ROTATION_RATE*TIME_STEP_SIZE)/(4.0E0*ANGULAR_GRID_SPACING_SIZE),TIME_STEP_SIZE/(4.0E0*RADIUS**2*ANGULAR_GRID_SPACING_SIZE**2))
            MAIN_DIAGONAL = CMPLX(1.0E0,0.0E0)-CMPLX(0.0E0,TIME_STEP_SIZE/4.0E0)*(CMPLX(2.0E0/(RADIUS**2*ANGULAR_GRID_SPACING_SIZE**2),0.0E0)+CMPLX(2.0E0,0.0E0)*EXTERNAL_POTENTIAL+NONLINEAR_COUPLING*CMPLX(ABS(WAVEFUNCTION)**2+ABS(TEMPORARY_STORAGE)**2,0.0E0))
            LOWER_DIAGONAL = -CONJG(UPPER_DIAGONAL)
            RIGHT_HAND_SIDE(1) = UPPER_DIAGONAL(1)*TEMPORARY_STORAGE(2)+MAIN_DIAGONAL(1)*TEMPORARY_STORAGE(1)+LOWER_DIAGONAL(1)*TEMPORARY_STORAGE(NUMBER_OF_GRID_POINTS)
            DO CURRENT_GRID_POINT = 2, NUMBER_OF_GRID_POINTS-1
               RIGHT_HAND_SIDE(CURRENT_GRID_POINT) = UPPER_DIAGONAL(CURRENT_GRID_POINT)*TEMPORARY_STORAGE(CURRENT_GRID_POINT+1)+MAIN_DIAGONAL(CURRENT_GRID_POINT)*TEMPORARY_STORAGE(CURRENT_GRID_POINT)+LOWER_DIAGONAL(CURRENT_GRID_POINT-1)*TEMPORARY_STORAGE(CURRENT_GRID_POINT-1)
            ENDDO
            RIGHT_HAND_SIDE(NUMBER_OF_GRID_POINTS) = UPPER_DIAGONAL(NUMBER_OF_GRID_POINTS-1)*TEMPORARY_STORAGE(1)+MAIN_DIAGONAL(NUMBER_OF_GRID_POINTS)*TEMPORARY_STORAGE(NUMBER_OF_GRID_POINTS)+LOWER_DIAGONAL(NUMBER_OF_GRID_POINTS-1)*TEMPORARY_STORAGE(NUMBER_OF_GRID_POINTS-1)
            UPPER_DIAGONAL = -UPPER_DIAGONAL
            MAIN_DIAGONAL = CONJG(MAIN_DIAGONAL)
            LOWER_DIAGONAL = -LOWER_DIAGONAL
            W = CMPLX(0.0E0,0.0E0)
            Z = CMPLX(0.0E0,0.0E0)
            W(1) = CMPLX(1.0E0,0.0E0)
            W(NUMBER_OF_GRID_POINTS) = CMPLX(1.0E0,0.0E0)
            Z(1) = -UPPER_DIAGONAL(1)
            Z(NUMBER_OF_GRID_POINTS) = -LOWER_DIAGONAL(1)
            MAIN_DIAGONAL(1) = MAIN_DIAGONAL(1)+W(1)*Z(1)
            MAIN_DIAGONAL(NUMBER_OF_GRID_POINTS) = MAIN_DIAGONAL(NUMBER_OF_GRID_POINTS)+W(NUMBER_OF_GRID_POINTS)*Z(NUMBER_OF_GRID_POINTS)
            CALL ZGTSV(NUMBER_OF_GRID_POINTS,1,LOWER_DIAGONAL,MAIN_DIAGONAL,UPPER_DIAGONAL,RIGHT_HAND_SIDE,NUMBER_OF_GRID_POINTS,INFO)
            UPPER_DIAGONAL = -CMPLX((ROTATION_RATE*TIME_STEP_SIZE)/(4.0E0*ANGULAR_GRID_SPACING_SIZE),TIME_STEP_SIZE/(4.0E0*RADIUS**2*ANGULAR_GRID_SPACING_SIZE**2))
            MAIN_DIAGONAL = CMPLX(1.0E0,0.0E0)+CMPLX(0.0E0,TIME_STEP_SIZE/4.0E0)*(CMPLX(2.0E0/(RADIUS**2*ANGULAR_GRID_SPACING_SIZE**2),0.0E0)+CMPLX(2.0E0,0.0E0)*EXTERNAL_POTENTIAL+NONLINEAR_COUPLING*CMPLX(ABS(WAVEFUNCTION)**2+ABS(TEMPORARY_STORAGE)**2,0.0E0))
            LOWER_DIAGONAL = -CONJG(UPPER_DIAGONAL)
            MAIN_DIAGONAL(1) = MAIN_DIAGONAL(1)+W(1)*Z(1)
            MAIN_DIAGONAL(NUMBER_OF_GRID_POINTS) = MAIN_DIAGONAL(NUMBER_OF_GRID_POINTS)+W(NUMBER_OF_GRID_POINTS)*Z(NUMBER_OF_GRID_POINTS)
            CALL ZGTSV(NUMBER_OF_GRID_POINTS,1,LOWER_DIAGONAL,MAIN_DIAGONAL,UPPER_DIAGONAL,W,NUMBER_OF_GRID_POINTS,INFO)
            WAVEFUNCTION = RIGHT_HAND_SIDE+(SUM(Z*RIGHT_HAND_SIDE)/(CMPLX(1.0E0,0.0E0)-SUM(Z*W)))*W
         ENDDO
         IF (MODULO(CURRENT_TIME_STEP,NUMBER_OF_TIME_STEPS_BEFORE_WRITE).EQ.0) THEN
            OPEN(UNIT=CURRENT_FILE,ACTION='WRITE',FORM='UNFORMATTED')
            WRITE(UNIT=CURRENT_FILE) WAVEFUNCTION
            CLOSE(UNIT=CURRENT_FILE)
            CURRENT_FILE = CURRENT_FILE + 1
         ENDIF
      ENDDO

!     Deallocate Arrays:
!     ------------------
      DEALLOCATE(WAVEFUNCTION)
      DEALLOCATE(EXTERNAL_POTENTIAL)
      DEALLOCATE(NONLINEAR_COUPLING)
      DEALLOCATE(TEMPORARY_STORAGE)
      DEALLOCATE(RIGHT_HAND_SIDE)
      DEALLOCATE(UPPER_DIAGONAL)
      DEALLOCATE(MAIN_DIAGONAL)
      DEALLOCATE(LOWER_DIAGONAL)
      DEALLOCATE(W)
      DEALLOCATE(Z)

      STOP

      ENDPROGRAM GPECN1DTDPBC
! ======================================================================
