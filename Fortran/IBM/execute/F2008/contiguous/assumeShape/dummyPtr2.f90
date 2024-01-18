! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS attribute
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - Pointer has contiguous attribute
!*
!*    Dummy argument is pointer with CONTIGUOUS attribute
!*    Actual argument is contiguous
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
PROGRAM dummyPtr2
      IMPLICIT NONE

      INTEGER :: I, J
      INTEGER, TARGET  :: I2D(2,2)
      INTEGER, POINTER, CONTIGUOUS :: ptr(:,:)

      I2D = RESHAPE( SOURCE = [(I, I=1,4)], SHAPE = [2,2] )
      ptr => I2D
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 10

      CALL Sub(ptr)
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 11
      IF (ANY(ptr .NE. RESHAPE(SOURCE =[1,2,2,4], SHAPE=[2,2]) )) ERROR STOP 12

      CONTAINS

      SUBROUTINE Sub(Arg)
        INTEGER, POINTER, CONTIGUOUS  :: Arg(:,:)

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 20

        DO I = LBOUND(Arg, 1), UBOUND(Arg, 1), 1
           DO J = LBOUND(Arg, 2), UBOUND(Arg, 2), 1
                Arg(I,J) = I*J
           END DO
        END DO
      END SUBROUTINE Sub

END PROGRAM dummyPtr2
