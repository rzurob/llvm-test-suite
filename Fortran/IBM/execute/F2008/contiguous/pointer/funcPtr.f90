! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : Data pointer assingment
!*  SECONDARY FUNCTIONS TESTED : Function result with CONTIGUOUS attribute
!*
!*  DESCRIPTION                : - Pointer has CONTIGUOUS attribute
!*    - Dummy is assumed shape array with or without CONTIGUOUS attribute
!*    - Actual is contiguous array
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
PROGRAM funcPtr
      IMPLICIT NONE

      INTEGER  :: L, U, P, Q
      INTEGER  :: I3D(2,2,2)
      INTEGER, POINTER :: ptr(:,:,:)

      ptr => foo(I3D)
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 10

      ptr => foo(I3D(:,:,:))
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 11

      ptr => foo(I3D(:,:,1:))
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 12

      ptr => foo(I3D(1:,1:,:))
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 13

      ptr => foo(I3D(1:,1:,1:))
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 14

      ptr => foo(I3D(1:2,1:2,:))
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 15

      ptr => foo(I3D(1:2,1:2,1:2))
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 16

      ptr => foo(I3D(:,:,1:0))
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 17

      ptr => foo(I3D(:,:,1:1))
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 18

      ptr => foo(I3D(:,:,1:2))
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 19

      ptr => foo(I3D(:,:,1:2:1))
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 20

      L = LBOUND(I3D, 1)
      U = UBOUND(I3D, 1)
      P = LBOUND(I3D, 2)
      Q = UBOUND(I3D, 2)

      ptr => foo(I3D(:,:,L:U))
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 21

      ptr => foo(I3D(:,:,P:Q))
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 22

      ptr => foo(I3D(L:,:,:))
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 23

      ptr => foo(I3D(L:,L:,L:))
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 24

      ptr => foo(I3D(L:U,L:U,L:U))
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 25

      ptr => foo(I3D(LBOUND(I3D, 1):UBOUND(I3D, 1),   &
&                    LBOUND(I3D, 2):UBOUND(I3D, 2),   &
&                    LBOUND(I3D, 3):UBOUND(I3D, 3)))
      IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 26

      CONTAINS

      FUNCTION foo(Arg)
        INTEGER, TARGET, CONTIGUOUS  :: Arg(:,:,:)
        INTEGER, POINTER, CONTIGUOUS :: foo(:,:,:)

        IF (      ASSOCIATED(foo)     ) ERROR STOP 30
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 31

        foo => Arg

        IF ( .NOT. ASSOCIATED(foo)    ) ERROR STOP 32
        IF ( .NOT. IS_CONTIGUOUS(foo) ) ERROR STOP 33
      END FUNCTION foo

END PROGRAM funcPtr
