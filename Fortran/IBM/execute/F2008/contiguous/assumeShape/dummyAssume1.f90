! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : Data pointer assingment
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - Pointer has contiguous attribute
!*
!*    Dummy argument is pointer with no CONTIGUOUS attribute
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
MODULE Mod
      IMPLICIT NONE

      INTEGER, TARGET  :: I2D(2,2)
      INTEGER, POINTER :: ptr(:,:)

      CONTAINS

      SUBROUTINE Sub(Arg)
        INTEGER, TARGET, CONTIGUOUS  :: Arg(:,:)

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 11
        Arg = Arg + 1
      END SUBROUTINE Sub

      SUBROUTINE Sub2(Arg)
        INTEGER, TARGET, CONTIGUOUS, INTENT(OUT)  :: Arg(:,:)

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 21
        Arg = 0
      END SUBROUTINE Sub2
END MODULE Mod
PROGRAM dummyAssume1
      USE Mod

      I2D = RESHAPE( SOURCE = [(I, I=1,100)], SHAPE = [2,2] )
      ptr => I2D

      CALL Sub(ptr)
      IF (ANY(ptr .NE. RESHAPE(SOURCE =[(I, I=2,101)], SHAPE=[2,2]) )) ERROR STOP 100

      CALL Sub2(ptr)
      IF (ANY(ptr .NE. 0 )) ERROR STOP 101

END PROGRAM dummyAssume1
