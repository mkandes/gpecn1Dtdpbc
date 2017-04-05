! ======================================================================
! NAME
!
!     POTENTIAL
!
! DESCRIPTION
!
!     POTENTIAL is a supporting program of GPECN1DTDPBC that generates
!     time-independent external potential functions that may be used to
!     act on its wave functions.
!
! AUTHOR
!
!     Marty Kandes, Ph.D.
!     Computational Data Science Research Specialist
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
!     Tuesday, April 4th, 2017                                   
!
! ----------------------------------------------------------------------
      PROGRAM POTENTIAL
      IMPLICIT NONE

!     Parameter Declarations:
!     -----------------------
      REAL, PARAMETER :: PI = 3.1415926535897932384626433832795028841971E0

!     Input Variable and Array Declarations:
!     --------------------------------------
      INTEGER :: NUMBER_OF_GRID_POINTS
      INTEGER :: EXTERNAL_POTENTIAL_SWITCH
      
      REAL :: RADIUS
      REAL :: GAUSSIAN_OSCILLATOR_STRENGTH
      REAL :: INITIAL_ANGULAR_POSITION

!     Internal Variable and Array Declarations:
!     -----------------------------------------
      CHARACTER(80) :: BUFFER

      INTEGER :: CURRENT_GRID_POINT
      INTEGER :: LENGTH
      INTEGER :: INFO

      REAL :: ANGULAR_GRID_SPACING_SIZE

      COMPLEX, ALLOCATABLE, DIMENSION(:) :: EXTERNAL_POTENTIAL

!     Input Variable Assignment Read from Command-line Arguments:
!     -----------------------------------------------------------
      CALL GET_COMMAND_ARGUMENT(1,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) NUMBER_OF_GRID_POINTS

      CALL GET_COMMAND_ARGUMENT(2,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) EXTERNAL_POTENTIAL_SWITCH

      CALL GET_COMMAND_ARGUMENT(3,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) RADIUS

      CALL GET_COMMAND_ARGUMENT(4,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) GAUSSIAN_OSCILLATOR_STRENGTH

      CALL GET_COMMAND_ARGUMENT(5,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) INITIAL_ANGULAR_POSITION

!     Allocate Arrays:
!     ----------------
      ALLOCATE(EXTERNAL_POTENTIAL(NUMBER_OF_GRID_POINTS))

!     Internal Variable Assignment:
!     -----------------------------
      ANGULAR_GRID_SPACING_SIZE = 2.0E0*PI/FLOAT(NUMBER_OF_GRID_POINTS)

!     Main Program:
!     -------------
      IF (EXTERNAL_POTENTIAL_SWITCH.EQ.0) THEN
         EXTERNAL_POTENTIAL = CMPLX(0.0E0,0.0E0)
      ELSEIF (EXTERNAL_POTENTIAL_SWITCH.EQ.1) THEN
         DO CURRENT_GRID_POINT = 1, NUMBER_OF_GRID_POINTS
            EXTERNAL_POTENTIAL(CURRENT_GRID_POINT) = CMPLX(0.5E0*GAUSSIAN_OSCILLATOR_STRENGTH**2*RADIUS**2*(FLOAT(CURRENT_GRID_POINT-1)*ANGULAR_GRID_SPACING_SIZE-INITIAL_ANGULAR_POSITION)**2,0.0E0)
         ENDDO
      ENDIF

      OPEN(UNIT=999,ACTION='WRITE',FORM='UNFORMATTED')
      WRITE(UNIT=999) EXTERNAL_POTENTIAL
      CLOSE(UNIT=999) 

!     Deallocate Arrays:
!     ------------------
      DEALLOCATE(EXTERNAL_POTENTIAL)

      STOP
      ENDPROGRAM POTENTIAL
! ======================================================================
