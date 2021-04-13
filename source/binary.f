! ======================================================================
! NAME
!
!     BINARY
!
! DESCRIPTION
!
!     BINARY is a supporting program of GPECN1DTDPBC that converts its 
!     unformatted, binary output wave function files to a simple 
!     formatted, ASCII file.
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
!     Tuesday, April 4th, 2017                                   
!
! ----------------------------------------------------------------------
      PROGRAM BINARY
      IMPLICIT NONE

!     Parameter Declarations:
!     -----------------------
      REAL, PARAMETER :: PI = 3.1415926535897932384626433832795028841971E0

!     Input Variable and Array Declarations:
!     --------------------------------------
      INTEGER :: NUMBER_OF_GRID_POINTS
      INTEGER :: CURRENT_FILE
      INTEGER :: OUTPUT_SHIFT

      REAL :: RADIUS

      COMPLEX, ALLOCATABLE, DIMENSION(:) :: WAVEFUNCTION

!     Internal Variable and Array Declarations:
!     -----------------------------------------
      CHARACTER(80) :: BUFFER

      INTEGER :: CURRENT_GRID_POINT
      INTEGER :: LENGTH
      INTEGER :: INFO

      REAL :: ANGULAR_GRID_SPACING_SIZE

!     Input Variable Assignment Read from Command-line Arguments:
!     -----------------------------------------------------------
      CALL GET_COMMAND_ARGUMENT(1,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) NUMBER_OF_GRID_POINTS

      CALL GET_COMMAND_ARGUMENT(2,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) CURRENT_FILE

      CALL GET_COMMAND_ARGUMENT(3,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) OUTPUT_SHIFT

      CALL GET_COMMAND_ARGUMENT(4,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) RADIUS

!     Allocate Arrays:
!     ----------------
      ALLOCATE(WAVEFUNCTION(NUMBER_OF_GRID_POINTS))

!     Internal Variable Assignment:
!     -----------------------------
      ANGULAR_GRID_SPACING_SIZE = 2.0E0*PI/FLOAT(NUMBER_OF_GRID_POINTS)

!     Read Input Arrays from File:
!     ----------------------------
      OPEN(UNIT=CURRENT_FILE,ACTION='READ',FORM='UNFORMATTED')
      READ(UNIT=CURRENT_FILE) WAVEFUNCTION
      CLOSE(UNIT=CURRENT_FILE)

!     Write Converted Output to File:
!     -------------------------------
      OPEN(UNIT=CURRENT_FILE+OUTPUT_SHIFT,ACTION='WRITE',FORM='FORMATTED')
      DO CURRENT_GRID_POINT = 1, NUMBER_OF_GRID_POINTS
         WRITE(CURRENT_FILE+OUTPUT_SHIFT,900) RADIUS*FLOAT(CURRENT_GRID_POINT-1)*ANGULAR_GRID_SPACING_SIZE, ABS(WAVEFUNCTION(CURRENT_GRID_POINT))**2, REAL(WAVEFUNCTION(CURRENT_GRID_POINT)), AIMAG(WAVEFUNCTION(CURRENT_GRID_POINT))
      ENDDO
      CLOSE(UNIT=CURRENT_FILE+OUTPUT_SHIFT)

!     Format Statements:
!     ------------------
900   FORMAT(1X,4(F23.15))


!     Deallocate Arrays:
!     ------------------
      DEALLOCATE(WAVEFUNCTION)

      STOP
      ENDPROGRAM BINARY
! ======================================================================
