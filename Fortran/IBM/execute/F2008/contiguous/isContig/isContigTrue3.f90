! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : IS_CONTIGUOUS
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : -
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
PROGRAM isContigTrue3
      IMPLICIT NONE

      INTEGER  :: I, I3D(5,5,5)
      INTEGER, POINTER, CONTIGUOUS  :: ptrc(:,:,:)

      I3D = RESHAPE( SOURCE = [(I, I=1,125)], SHAPE = [5,5,5] )

      CALL Sub(I3D)
      CALL Sub(I3D(:,:,:))
      CALL Sub(I3D(:,:,1:))
      CALL Sub(I3D(1:,1:,:))
      CALL Sub(I3D(1:,1:,1:))
      CALL Sub(I3D(1:5,1:5,:))
      CALL Sub(I3D(1:5,1:5,1:5))
      CALL Sub(I3D(:,:,1:5))
      CALL Sub(I3D(:,:,1:5:1))

      ALLOCATE( ptrc(5,5,5), SOURCE = I3D )

      CALL Sub(ptrc)
      CALL Sub(ptrc(:,:,:))
      CALL Sub(ptrc(:,:,1:))
      CALL Sub(ptrc(1:,1:,:))
      CALL Sub(ptrc(1:,1:,1:))
      CALL Sub(ptrc(1:5,1:5,:))
      CALL Sub(ptrc(1:5,1:5,1:5))
      CALL Sub(ptrc(:,:,1:5))
      CALL Sub(ptrc(:,:,1:5:1))

      CONTAINS

      SUBROUTINE Sub(Arg)
        INTEGER :: Arg(:,:,:)

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 11
      END SUBROUTINE Sub
END PROGRAM isContigTrue3
