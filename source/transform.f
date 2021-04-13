! ======================================================================
! NAME
!
!     TRANSFORM
!
! DESCRIPTION
!
!     TRANSFORM is a supporting program of GPECN1DTDPBC that computes 
!     the doppler-shifted wavefunction (from a rotating frame to the 
!     inertial frame).
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
!     Wednesday, July 5th, 2017                                   
!
! ----------------------------------------------------------------------
      PROGRAM TRANSFORM
      IMPLICIT NONE

!     Parameter Declarations:
!     -----------------------
      REAL, PARAMETER :: PI = 3.1415926535897932384626433832795028841971E0

!     Input Variable and Array Declarations:
!     --------------------------------------
      INTEGER :: NUMBER_OF_GRID_POINTS
      INTEGER :: NUMBER_OF_TIME_STEPS
      INTEGER :: NUMBER_OF_TIME_STEPS_BEFORE_WRITE
      INTEGER :: CURRENT_FILE
      INTEGER :: OUTPUT_SHIFT

      REAL :: RADIUS
      REAL :: ROTATION_RATE
      REAL :: GAMMA_FACTOR

      COMPLEX, ALLOCATABLE, DIMENSION(:) :: WAVEFUNCTION

!     Internal Variable and Array Declarations:
!     -----------------------------------------
      CHARACTER(80) :: BUFFER

      INTEGER :: CURRENT_GRID_POINT
      INTEGER :: CURRENT_TIME_STEP
      INTEGER :: LENGTH
      INTEGER :: INFO

      REAL :: ANGULAR_GRID_SPACING_SIZE
      REAL :: TIME_STEP_SIZE

!     Input Variable Assignment Read from Command-line Arguments:
!     -----------------------------------------------------------
      CALL GET_COMMAND_ARGUMENT(1,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) NUMBER_OF_GRID_POINTS

      CALL GET_COMMAND_ARGUMENT(2,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) NUMBER_OF_TIME_STEPS

      CALL GET_COMMAND_ARGUMENT(3,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) NUMBER_OF_TIME_STEPS_BEFORE_WRITE

      CALL GET_COMMAND_ARGUMENT(4,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) CURRENT_FILE

      CALL GET_COMMAND_ARGUMENT(5,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) OUTPUT_SHIFT

      CALL GET_COMMAND_ARGUMENT(6,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) RADIUS

      CALL GET_COMMAND_ARGUMENT(7,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) ROTATION_RATE

      CALL GET_COMMAND_ARGUMENT(8,BUFFER,LENGTH,INFO)
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
      OPEN(UNIT=CURRENT_FILE,ACTION='READ',FORM='UNFORMATTED')
      READ(UNIT=CURRENT_FILE) WAVEFUNCTION
      CLOSE(UNIT=CURRENT_FILE)

      CURRENT_TIME_STEP = MODULO(CURRENT_FILE,1000) + 1

!     Transform Wavefunction from Rotating Frame to Inertial Frame
!     ------------------------------------------------------------
      DO CURRENT_GRID_POINT = 1, NUMBER_OF_GRID_POINTS
         WAVEFUNCTION(CURRENT_GRID_POINT) = EXP(CMPLX(0.0,(ROTATION_RATE*RADIUS**2*FLOAT(CURRENT_GRID_POINT-1)*ANGULAR_GRID_SPACING_SIZE)-(0.5*ROTATION_RATE**2*RADIUS**2*FLOAT(CURRENT_TIME_STEP-1)*TIME_STEP_SIZE)))*WAVEFUNCTION(CURRENT_GRID_POINT)
      ENDDO

!     Write Converted Output to File:
!     -------------------------------
      OPEN(UNIT=CURRENT_FILE+OUTPUT_SHIFT,ACTION='WRITE',FORM='UNFORMATTED')
         WRITE(UNIT=CURRENT_FILE+OUTPUT_SHIFT) WAVEFUNCTION
      CLOSE(UNIT=CURRENT_FILE+OUTPUT_SHIFT)

!     Format Statements:
!     ------------------
900   FORMAT(1X,4(F23.15))

!     Deallocate Arrays:
!     ------------------
      DEALLOCATE(WAVEFUNCTION)

      STOP
      ENDPROGRAM TRANSFORM
! ======================================================================
