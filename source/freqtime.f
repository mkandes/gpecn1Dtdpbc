! ======================================================================
! NAME
!
!     FREQTIME
!
! DESCRIPTION
!
!     FREQTIME is a supporting program of GPECN1DTDPBC that computes the
!     coeffecients of the Fourier transform of its output wave functions'
!     and organizes these amplitudes into a spacetime-like plot, but in 
!     frequency space.
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
      PROGRAM FREQTIME
      IMPLICIT NONE

!     Parameter Declarations:
!     -----------------------
      REAL, PARAMETER :: PI = 3.1415926535897932384626433832795028841971E0

!     Input Variable and Array Declarations:
!     --------------------------------------
      INTEGER :: NUMBER_OF_GRID_POINTS
      INTEGER :: NUMBER_OF_TIME_STEPS
      INTEGER :: NUMBER_OF_TIME_STEPS_BEFORE_WRITE
      INTEGER :: START_FILE
      INTEGER :: END_FILE
      INTEGER :: OUTPUT_FILE
      INTEGER :: START_MOMENTUM
      INTEGER :: END_MOMENTUM

      REAL :: RADIUS
      REAL :: GAMMA_FACTOR

      COMPLEX, ALLOCATABLE, DIMENSION(:) :: WAVEFUNCTION(:)

!     Internal Variable and Array Declarations:
!     -----------------------------------------
      CHARACTER(80) :: BUFFER

      INTEGER :: CURRENT_FILE
      INTEGER :: CURRENT_GRID_POINT
      INTEGER :: CURRENT_MOMENTUM
      INTEGER :: LENGTH
      INTEGER :: INFO

      REAL :: ANGULAR_GRID_SPACING_SIZE
      REAL :: TIME_STEP_SIZE
      REAL :: TIME
      REAL :: FOURIER_COEFFICIENT

!     Input Variable Assignment Read from Command-line Arguments:
!     -----------------------------------------------------------
      CALL GET_COMMAND_ARGUMENT(1,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) NUMBER_OF_GRID_POINTS

      CALL GET_COMMAND_ARGUMENT(2,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) NUMBER_OF_TIME_STEPS

      CALL GET_COMMAND_ARGUMENT(3,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) NUMBER_OF_TIME_STEPS_BEFORE_WRITE

      CALL GET_COMMAND_ARGUMENT(4,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) START_FILE

      CALL GET_COMMAND_ARGUMENT(5,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) END_FILE

      CALL GET_COMMAND_ARGUMENT(6,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) OUTPUT_FILE

      CALL GET_COMMAND_ARGUMENT(7,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) START_MOMENTUM

      CALL GET_COMMAND_ARGUMENT(8,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) END_MOMENTUM

      CALL GET_COMMAND_ARGUMENT(9,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) RADIUS

      CALL GET_COMMAND_ARGUMENT(10,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) GAMMA_FACTOR

!     Allocate Arrays:
!     ----------------
      ALLOCATE(WAVEFUNCTION(NUMBER_OF_GRID_POINTS))

!     Internal Variable Assignment:
!     -----------------------------
      ANGULAR_GRID_SPACING_SIZE = 2.0E0*PI/FLOAT(NUMBER_OF_GRID_POINTS)
      TIME_STEP_SIZE = (RADIUS*ANGULAR_GRID_SPACING_SIZE)**2/(2.0E0*GAMMA_FACTOR)

!     Read Input Arrays from File:
!     ----------------------------
      OPEN(UNIT=OUTPUT_FILE,ACTION='WRITE',FORM='FORMATTED')
      DO CURRENT_FILE = START_FILE, END_FILE
         OPEN(UNIT=CURRENT_FILE,ACTION='READ',FORM='UNFORMATTED')
            READ(UNIT=CURRENT_FILE) WAVEFUNCTION
         CLOSE(UNIT=CURRENT_FILE)
         TIME = FLOAT(CURRENT_FILE-START_FILE)*FLOAT(NUMBER_OF_TIME_STEPS_BEFORE_WRITE)*TIME_STEP_SIZE
         DO CURRENT_MOMENTUM = START_MOMENTUM, END_MOMENTUM
            FOURIER_COEFFICIENT = CMPLX(0.0E0,0.0E0)
            DO CURRENT_GRID_POINT = 1, NUMBER_OF_GRID_POINTS
               FOURIER_COEFFICIENT=FOURIER_COEFFICIENT+WAVEFUNCTION(CURRENT_GRID_POINT)*EXP(CMPLX(0.0E0,-FLOAT(CURRENT_MOMENTUM)*FLOAT(CURRENT_GRID_POINT-1)*ANGULAR_GRID_SPACING_SIZE))
            ENDDO
            FOURIER_COEFFICIENT=FOURIER_COEFFICIENT*CMPLX(RADIUS*ANGULAR_GRID_SPACING_SIZE/SQRT(2.0E0*PI*RADIUS),0.0E0)
            WRITE(OUTPUT_FILE,*) TIME, CURRENT_MOMENTUM, ABS(FOURIER_COEFFICIENT)**2
         ENDDO
         WRITE(OUTPUT_FILE,*) ' '
      ENDDO
      CLOSE(UNIT=OUTPUT_FILE)

!     Deallocate Arrays:
!     ------------------
      DEALLOCATE(WAVEFUNCTION)

      STOP
      ENDPROGRAM FREQTIME
! =========================================================================

