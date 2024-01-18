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
!*    LHS is array pointer with CONTIGUOUS attribute (rank = 5)
!*    RHS is array pointer with CONTIGUOUS attribute (rank = 5)
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
PROGRAM bndMapping4
      IMPLICIT NONE

      INTEGER  :: I, J
      INTEGER, TARGET  :: I5D(5,5,5,5,5)
      INTEGER, POINTER, CONTIGUOUS :: ptr1(:,:,:,:,:), ptr2(:,:,:,:,:)

      I5D =  RESHAPE(SOURCE = [(I, I=1,3125)], SHAPE = [5,5,5,5,5])
      IF ( .NOT. IS_CONTIGUOUS(I5D) )   ERROR STOP 10

! Pointer assignment
      ptr1(1:5,1:5,1:5,1:5,1:5) => I5D
      IF ( .NOT. IS_CONTIGUOUS(ptr1) )  ERROR STOP 11
      IF ( ANY(ptr1 .NE. I5D) ) ERROR STOP 10

      ptr2(1:5,1:5,1:5,1:5,1:5) => ptr1
      IF ( .NOT. IS_CONTIGUOUS(ptr2) )  ERROR STOP 12
      IF ( ANY(ptr2 .NE. I5D) )         ERROR STOP 13

      ptr2(1:UBOUND(ptr1,5),1:UBOUND(ptr1,4),1:UBOUND(ptr1,3),1:UBOUND(ptr1,2),1:UBOUND(ptr1,1)) => ptr1
      IF ( .NOT. IS_CONTIGUOUS(ptr2) )  ERROR STOP 14
      IF ( ANY(ptr2 .NE. I5D) )         ERROR STOP 15

      ptr2(LBOUND(ptr1,1):5,LBOUND(ptr1,2):5,LBOUND(ptr1,3):5,LBOUND(ptr1,4):5,LBOUND(ptr1,5):5) => ptr1
      IF ( .NOT. IS_CONTIGUOUS(ptr2) )  ERROR STOP 16
      IF ( ANY(ptr2 .NE. I5D) )         ERROR STOP 17

      I = LBOUND(ptr1,1); J = UBOUND(ptr1,1)
      ptr2(I:J,I:J,I:J,I:J,I:J) => ptr1
      IF ( .NOT. IS_CONTIGUOUS(ptr2) )  ERROR STOP 18
      IF ( ANY(ptr2 .NE. I5D) )         ERROR STOP 19

      ptr2(I:J,I+1:J,I+2:J-1,I:J-2,I:J-3) => ptr1
      IF ( .NOT. IS_CONTIGUOUS(ptr2) )  ERROR STOP 20
      IF ( ANY(ptr2 .NE. RESHAPE(SOURCE=[(I, I=1,240)], SHAPE = [5,4,2,3,2])) )  ERROR STOP 21

! Subroutine calls
      CALL Sub1D(I5D, 1)

! Function calls
      IF (ANY(Fnc2D(I5D,10,10) .NE. RESHAPE(SOURCE=[(I, I=1,100)], SHAPE=[10,10]) )) ERROR STOP 22
      IF ( .NOT. IS_CONTIGUOUS(Fnc2D(I5D,10,10)) )  ERROR STOP 23

      CONTAINS

      SUBROUTINE Sub1D(Arg, d)
        INTEGER, TARGET, CONTIGUOUS :: Arg(:,:,:,:,:)
        INTEGER, POINTER :: ptr(:)
        INTEGER :: d, up, low

        up = UBOUND(Arg, d); low = LBOUND(Arg, d)
        ptr(low:up) => Arg
        IF ( .NOT.  IS_CONTIGUOUS(ptr) )  ERROR STOP 30
        IF ( ANY(ptr .NE. [1,2,3,4,5]) )  ERROR STOP 31
      END SUBROUTINE Sub1D

      FUNCTION Fnc2D(Arg, K, P)
          INTEGER :: K, P, s
          INTEGER, TARGET, CONTIGUOUS :: Arg(:,:,:,:,:)
          INTEGER, POINTER, CONTIGUOUS  :: Fnc2D(:,:)

          s = K * P
          IF ( s > SIZE(Arg) ) THEN
              ERROR STOP 20
          ELSE
              Fnc2D( 1:K,1:P ) => Arg
          ENDIF
      END FUNCTION Fnc2D

END PROGRAM bndMapping4
