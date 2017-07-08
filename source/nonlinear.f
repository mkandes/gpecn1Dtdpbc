! ======================================================================
! NAME
!
!     NONLINEAR
!
! DESCRIPTION
!
!     NONLINEAR is a supporting program of GPECN1DTDPBC that generates
!     a uniform, time-independent, and real-valued coupling constant 
!     that parameterizes the strength of a (simple, single-component) 
!     Bose-Einstein condensate’s nonlinear mean-field interaction 
!     arising from two-body contact-like interactions between atoms 
!     within the condensate.
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
!     Tuesday, April 4th, 2017                                   
!
! ----------------------------------------------------------------------
      PROGRAM NONLINEAR
      IMPLICIT NONE

!     Parameter Declarations:
!     -----------------------
      REAL, PARAMETER :: PI = 3.1415926535897932384626433832795028841971E0

!     Input Variable and Array Declarations:
!     --------------------------------------
      INTEGER :: NUMBER_OF_GRID_POINTS
      INTEGER :: NONLINEAR_COUPLING_SWITCH

      REAL :: RADIUS
      REAL :: NONLINEAR_COUPLING_AMPLITUDE

!     Internal Variable and Array Declarations:
!     -----------------------------------------
      CHARACTER(80) :: BUFFER

      INTEGER :: CURRENT_GRID_POINT
      INTEGER :: LENGTH
      INTEGER :: INFO

      REAL :: ANGULAR_GRID_SPACING_SIZE

      COMPLEX, ALLOCATABLE, DIMENSION(:) :: NONLINEAR_COUPLING

!     Input Variable Assignment Read from Command-line Arguments:
!     -----------------------------------------------------------
      CALL GET_COMMAND_ARGUMENT(1,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) NUMBER_OF_GRID_POINTS

      CALL GET_COMMAND_ARGUMENT(2,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) NONLINEAR_COUPLING_SWITCH

      CALL GET_COMMAND_ARGUMENT(3,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) RADIUS

      CALL GET_COMMAND_ARGUMENT(4,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) NONLINEAR_COUPLING_AMPLITUDE

!     Allocate Arrays:
!     ----------------
      ALLOCATE(NONLINEAR_COUPLING(NUMBER_OF_GRID_POINTS))

!     Internal Variable Assignment:
!     -----------------------------
      ANGULAR_GRID_SPACING_SIZE = 2.0E0*PI/FLOAT(NUMBER_OF_GRID_POINTS)

!     Main Program:
!     -------------
      IF (NONLINEAR_COUPLING_SWITCH.EQ.0) THEN
         NONLINEAR_COUPLING = CMPLX(NONLINEAR_COUPLING_AMPLITUDE,0.0E0)
      ENDIF

      OPEN(UNIT=998,ACTION='WRITE',FORM='UNFORMATTED')
      WRITE(UNIT=998) NONLINEAR_COUPLING
      CLOSE(UNIT=998)

!     Deallocate Arrays:
!     ------------------
      DEALLOCATE(NONLINEAR_COUPLING)

      STOP
      ENDPROGRAM NONLINEAR
! ======================================================================
