! ======================================================================
! NAME
!
!     MOMENTUM
!
! DESCRIPTION
!
!     MOMENTUM is a supporting program of GPECN1DTDPBC that computes the
!     coeffecients of the Fourier transform of its output wave functions'
!     to produce a momentum distribution for each simulation.
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
!     Copyright (c) 2010, 2011, 2017 Martin Charles Kandes
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
      PROGRAM MOMENTUM
      IMPLICIT NONE

!     Parameter Declarations:
!     -----------------------
      REAL, PARAMETER :: PI = 3.1415926535897932384626433832795028841971E0

!     Input Variable and Array Declarations:
!     --------------------------------------
      INTEGER :: NUMBER_OF_GRID_POINTS
      INTEGER :: CURRENT_FILE
      INTEGER :: OUTPUT_SHIFT
      INTEGER :: START_MOMENTUM
      INTEGER :: END_MOMENTUM

      REAL :: RADIUS

!     Internal Variable and Array Declarations:
!     -----------------------------------------
      CHARACTER(80) :: BUFFER

      INTEGER :: CURRENT_GRID_POINT
      INTEGER :: CURRENT_MOMENTUM
      INTEGER :: LENGTH
      INTEGER :: INFO

      REAL :: ANGULAR_GRID_SPACING_SIZE

      COMPLEX :: FOURIER_COEFFICIENT

      COMPLEX, ALLOCATABLE, DIMENSION(:) :: WAVEFUNCTION

!     Input Variable Assignment Read from Command-line Arguments:
!     -----------------------------------------------------------
      CALL GET_COMMAND_ARGUMENT(1,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) NUMBER_OF_GRID_POINTS

      CALL GET_COMMAND_ARGUMENT(2,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) CURRENT_FILE

      CALL GET_COMMAND_ARGUMENT(3,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) OUTPUT_SHIFT

      CALL GET_COMMAND_ARGUMENT(4,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) START_MOMENTUM

      CALL GET_COMMAND_ARGUMENT(5,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) END_MOMENTUM

      CALL GET_COMMAND_ARGUMENT(6,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) RADIUS

!     Internal Variable Assignment:
!     -----------------------------
      ANGULAR_GRID_SPACING_SIZE = 2.0E0*PI/FLOAT(NUMBER_OF_GRID_POINTS)

!     Allocate Arrays:
!     ----------------
      ALLOCATE(WAVEFUNCTION(NUMBER_OF_GRID_POINTS))

!     Main Program:
!     -------------
      OPEN(UNIT=CURRENT_FILE,ACTION='READ',FORM='UNFORMATTED')
      READ(UNIT=CURRENT_FILE) WAVEFUNCTION
      CLOSE(UNIT=CURRENT_FILE)
      OPEN(UNIT=CURRENT_FILE+OUTPUT_SHIFT,ACTION='WRITE',FORM='FORMATTED')
      DO CURRENT_MOMENTUM = START_MOMENTUM, END_MOMENTUM
         FOURIER_COEFFICIENT = CMPLX(0.0E0,0.0E0)
         DO CURRENT_GRID_POINT = 1, NUMBER_OF_GRID_POINTS
            FOURIER_COEFFICIENT = FOURIER_COEFFICIENT+WAVEFUNCTION(CURRENT_GRID_POINT)*EXP(CMPLX(0.0E0,-FLOAT(CURRENT_MOMENTUM)*FLOAT(CURRENT_GRID_POINT-1)*ANGULAR_GRID_SPACING_SIZE))
         ENDDO
         FOURIER_COEFFICIENT=FOURIER_COEFFICIENT*CMPLX(RADIUS*ANGULAR_GRID_SPACING_SIZE/SQRT(2.0E0*PI*RADIUS),0.0E0)               
         WRITE(CURRENT_FILE+OUTPUT_SHIFT,900) CURRENT_MOMENTUM, ABS(FOURIER_COEFFICIENT)**2, REAL(FOURIER_COEFFICIENT), AIMAG(FOURIER_COEFFICIENT)
      ENDDO
      CLOSE(UNIT=CURRENT_FILE+OUTPUT_SHIFT)

!     Format Statements:
!     ------------------
900   FORMAT(1X,1(I5),3(F23.15))

!     Deallocate Arrays:
!     ------------------
      DEALLOCATE(WAVEFUNCTION)


      STOP
      ENDPROGRAM MOMENTUM
! ======================================================================
