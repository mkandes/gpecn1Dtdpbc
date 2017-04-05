! ======================================================================
! PROGRAM SLICEEXTRACT
!
!     Extracts 1D slice data around collision points from Michael's
!     psislicexyXXXXX.dat data files. We will use these 1D slices to 
!     extract the Sagnac phase shift rather than the integrated 1D 
!     Cartesian densities we already tried to use. This apporach here 
!     should hopefully give us clearer results and allow us to more 
!     directly/fairly compare 1D/2D simulation results.
!
! ----------------------------------------------------------------------
      PROGRAM SLICEEXTRACT
      IMPLICIT NONE

!     Input Variable and Array Declarations:
!     --------------------------------------
      INTEGER :: NUMBER_OF_GRID_POINTS
      INTEGER :: SLICE_ONE_START_LINE
      INTEGER :: SLICE_TWO_START_LINE
      INTEGER :: SLICE_ONE_FINISH_LINE
      INTEGER :: SLICE_TWO_FINISH_LINE
      INTEGER :: CURRENT_FILE
      INTEGER :: SLICE_ONE_FILE_SHIFT
      INTEGER :: SLICE_TWO_FILE_SHIFT

!     Internal Variable and Array Declarations:
!     -----------------------------------------
      CHARACTER(80) :: BUFFER

      INTEGER :: CURRENT_GRID_POINT
      INTEGER :: LENGTH
      INTEGER :: INFO
      INTEGER :: I

      REAL, ALLOCATABLE, DIMENSION (:) :: X
      REAL, ALLOCATABLE, DIMENSION (:) :: Y
      REAL, ALLOCATABLE, DIMENSION (:) :: DENSITY
      REAL, ALLOCATABLE, DIMENSION (:) :: PHASE

!     Input Variable Assignment Read from Command-line Arguments:
!     -----------------------------------------------------------
      CALL GET_COMMAND_ARGUMENT(1,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) NUMBER_OF_GRID_POINTS

      CALL GET_COMMAND_ARGUMENT(2,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) SLICE_ONE_START_LINE

      CALL GET_COMMAND_ARGUMENT(3,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) SLICE_TWO_START_LINE

      CALL GET_COMMAND_ARGUMENT(4,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) SLICE_ONE_FINISH_LINE

      CALL GET_COMMAND_ARGUMENT(5,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) SLICE_TWO_FINISH_LINE

      CALL GET_COMMAND_ARGUMENT(6,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) CURRENT_FILE

      CALL GET_COMMAND_ARGUMENT(7,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) SLICE_ONE_FILE_SHIFT

      CALL GET_COMMAND_ARGUMENT(8,BUFFER,LENGTH,INFO)
      READ(BUFFER,*) SLICE_TWO_FILE_SHIFT

!     Allocate Arrays:
!     ----------------
      ALLOCATE(X(NUMBER_OF_GRID_POINTS))
      ALLOCATE(Y(NUMBER_OF_GRID_POINTS))
      ALLOCATE(DENSITY(NUMBER_OF_GRID_POINTS))
      ALLOCATE(PHASE(NUMBER_OF_GRID_POINTS))

!     Read Slice One from File:
!     ----------------------------
      OPEN(UNIT=CURRENT_FILE,ACTION='READ',FORM='FORMATTED',STATUS='OLD')
      DO I = 1, SLICE_ONE_START_LINE-1
         READ(CURRENT_FILE,*)
      ENDDO
      DO I = SLICE_ONE_START_LINE, SLICE_ONE_FINISH_LINE
         READ(CURRENT_FILE,*) X(I-SLICE_ONE_START_LINE-1), Y(I-SLICE_ONE_START_LINE-1), DENSITY(I-SLICE_ONE_START_LINE-1), PHASE(I-SLICE_ONE_START_LINE-1)
      ENDDO
      CLOSE(UNIT=CURRENT_FILE,STATUS='KEEP')

!     Write Slice One File:
!     -------------------------------
      OPEN(UNIT=CURRENT_FILE+SLICE_ONE_FILE_SHIFT,ACTION='WRITE',FORM='FORMATTED',STATUS='NEW')
      DO I = 1, NUMBER_OF_GRID_POINTS
         WRITE(CURRENT_FILE+SLICE_ONE_FILE_SHIFT,900) X(I), DENSITY(I), PHASE(I)
      ENDDO
      CLOSE(UNIT=CURRENT_FILE+SLICE_ONE_FILE_SHIFT,STATUS='SAVE')

!     Read Slice Two from File:
!     ----------------------------
      OPEN(UNIT=CURRENT_FILE,ACTION='READ',FORM='FORMATTED',STATUS='OLD')
      DO I = 1, SLICE_TWO_START_LINE-1
         READ(CURRENT_FILE,*)
      ENDDO
      DO I = SLICE_TWO_START_LINE, SLICE_TWO_FINISH_LINE
         READ(CURRENT_FILE,*) X(I-SLICE_TWO_START_LINE-1), Y(I-SLICE_TWO_START_LINE-1), DENSITY(I-SLICE_TWO_START_LINE-1), PHASE(I-SLICE_TWO_START_LINE-1)                         
      ENDDO                  
      CLOSE(UNIT=CURRENT_FILE,STATUS='KEEP')
                             
!     Write Slice Two File:  
!     -------------------------------
      OPEN(UNIT=CURRENT_FILE+SLICE_TWO_FILE_SHIFT,ACTION='WRITE',FORM='FORMATTED',STATUS='NEW')
      DO I = 1, NUMBER_OF_GRID_POINTS
         WRITE(CURRENT_FILE+SLICE_TWO_FILE_SHIFT,900) X(I), DENSITY(I), PHASE(I)
      ENDDO                  
      CLOSE(UNIT=CURRENT_FILE+SLICE_TWO_FILE_SHIFT,STATUS='SAVE')

!     Format Statements:
!     ------------------
900   FORMAT(1X,3(F23.15))


!     Deallocate Arrays:
!     ------------------
      DEALLOCATE(X,Y,DENSITY,PHASE)

      STOP
      ENDPROGRAM SLICEEXTRACT
! =========================================================================
