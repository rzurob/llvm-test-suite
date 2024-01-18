! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : Bound Remapping
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : -
!*
!*    LHS is array pointer with CONTIGUOUS attribute (rank =1)
!*    RHS is assumed-shape array with CONTIGUOUS attribute (rank =3)
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
PROGRAM bndMapping3
      IMPLICIT NONE

      INTEGER, TARGET  :: I, I3D(2,2,2)
      INTEGER, POINTER :: ptr(:)

      I3D =  RESHAPE(SOURCE = [(I, I=1,8)], SHAPE = [2,2,2])

      CALL Sub(I3D)

      ptr => foo(I3D,1,8)
      IF (ANY(ptr .NE. [(I, I=1,8)])) ERROR STOP 10
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 11

      ptr => foo(I3D,1,5)
      IF (ANY(ptr .NE. [(I, I=1,5)])) ERROR STOP 12
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 13

      ptr => foo(I3D,2,5)
      IF (ANY(ptr .NE. [(I, I=1,4)])) ERROR STOP 14
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 15

      ptr => foo(I3D,7,8)
      IF (ANY(ptr .NE. [(I, I=1,2)])) ERROR STOP 16
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 17

      CONTAINS

      SUBROUTINE Sub(Arg)
        INTEGER, TARGET, CONTIGUOUS :: Arg(:,:,:)
        INTEGER, POINTER, CONTIGUOUS  :: ptrc(:)
        INTEGER :: Iup, Ilow

        Iup = UBOUND(Arg, 1)
        Ilow = LBOUND(Arg, 1)
        ptrc(Ilow:Iup) => Arg

        IF (ANY(ptrc .NE. [1,2])) ERROR STOP 18

      END SUBROUTINE Sub

      FUNCTION foo(Arg, K, P)
          INTEGER :: K, P, Isize
          INTEGER, POINTER :: foo(:)
          INTEGER, TARGET, CONTIGUOUS :: Arg(:,:,:)

              IF ( (P-K+1) > size(Arg) ) ERROR STOP 19
              foo(K:P) => Arg

      END FUNCTION foo
END PROGRAM bndMapping3
