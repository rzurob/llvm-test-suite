! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS attribute
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - Pointer has contiguous attribute
!*
!*    Dummy argument is pointer with CONTIGUOUS attribute
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
PROGRAM pointerAllocate
      IMPLICIT NONE

      INTEGER :: I
      INTEGER, POINTER, CONTIGUOUS :: ptr(:,:,:)

      CALL Sub(ptr)
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 11
      IF (ANY(ptr .NE. RESHAPE(SOURCE =[1,2,2,4,2,4,4,8], SHAPE=[2,2,2]) )) ERROR STOP 12

      CONTAINS

      SUBROUTINE Sub(Arg)
        INTEGER, POINTER, CONTIGUOUS  :: Arg(:,:,:)
        INTEGER :: I, J, K

        ALLOCATE( Arg(2,2,2), SOURCE = RESHAPE(SOURCE=[(I, I=1,8)], SHAPE = [2,2,2]) )
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 20

        DO I = LBOUND(Arg, 1), UBOUND(Arg, 1)
           DO J = LBOUND(Arg, 2), UBOUND(Arg, 2)
              DO K = LBOUND(Arg, 3), UBOUND(Arg, 3)
                Arg(I,J,K) = I*J*K
              END DO
           END DO
        END DO
      END SUBROUTINE Sub

END PROGRAM pointerAllocate
