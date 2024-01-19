! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*
!*  PRIMARY FUNCTIONS TESTED   : Contiguous dummy argfument
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
PROGRAM dummyAssume1
      INTEGER  :: I2D(1024)
      INTEGER, POINTER :: ptr(:)

      I2D = [(I, I=1,1024)]

      CALL Sub(I2D, ptr, 1, 1024, 1)
      IF (ANY(ptr .NE. [(I, I=1,1024)])) ERROR STOP 10

      CALL Sub(I2D, ptr, 1, 100, 1)
      IF (ANY(ptr .NE. [(I, I=1,100)])) ERROR STOP 11

      CALL Sub(I2D, ptr, 85, 86, 1)
      IF (ANY(ptr .NE. [(I, I=85,86,1)])) ERROR STOP 11

      CONTAINS

      SUBROUTINE Sub(Arg, Ptr, I, J, K)
        INTEGER, CONTIGUOUS  :: Arg(:)
        INTEGER, POINTER, INTENT(OUT) :: ptr(:)

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 11
        ALLOCATE(ptr(J-I+1), SOURCE = Arg(I:J:K))
      END SUBROUTINE Sub
END PROGRAM dummyAssume1
