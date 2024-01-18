! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : Data pointer assingment
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : - Target is dummy argument associated with
!*                                 contiguous actal argument
!*
!*    Dummy is assumed shape array with CONTIGUOUS attribute
!*    Actual is contiguous array
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
PROGRAM arraySectTar2
      IMPLICIT NONE

      INTEGER  :: L, U, P, Q
      INTEGER  :: I3D(2,2,2)
      INTEGER, POINTER, CONTIGUOUS  :: ptrc(:,:,:)

      CALL Sub1(I3D)
      CALL Sub1(I3D(:,:,:))
      CALL Sub1(I3D(:,:,1:))
      CALL Sub1(I3D(1:,1:,:))
      CALL Sub1(I3D(1:,1:,1:))
      CALL Sub1(I3D(1:2,1:2,:))
      CALL Sub1(I3D(1:2,1:2,1:2))
      CALL Sub1(I3D(:,:,1:0))
      CALL Sub1(I3D(:,:,1:1))
      CALL Sub1(I3D(:,:,1:2))
      CALL Sub1(I3D(:,:,1:2:1))

      L = LBOUND(I3D, 1)
      U = UBOUND(I3D, 1)
      P = LBOUND(I3D, 2)
      Q = UBOUND(I3D, 2)

      CALL Sub1(I3D(:,:,L:U))
      CALL Sub1(I3D(:,:,P:Q))
      CALL Sub1(I3D(L:,:,:))
      CALL Sub1(I3D(L:,L:,L:))
      CALL Sub1(I3D(L:U,L:U,L:U))
      CALL Sub1(I3D(LBOUND(I3D, 1):UBOUND(I3D, 1),   &
&                   LBOUND(I3D, 2):UBOUND(I3D, 2),   &
&                   LBOUND(I3D, 3):UBOUND(I3D, 3)))

!*
      ALLOCATE( ptrc(2,2,2) )

      CALL Sub1(ptrc)
      CALL Sub1(ptrc(:,:,:))
      CALL Sub1(ptrc(:,:,1:))
      CALL Sub1(ptrc(1:,1:,:))
      CALL Sub1(ptrc(1:,1:,1:))
      CALL Sub1(ptrc(1:2,1:2,:))
      CALL Sub1(ptrc(1:2,1:2,1:2))
      CALL Sub1(ptrc(:,:,1:0))
      CALL Sub1(ptrc(:,:,1:1))
      CALL Sub1(ptrc(:,:,1:2))
      CALL Sub1(ptrc(:,:,1:2:1))

      L = LBOUND(ptrc, 1)
      U = UBOUND(ptrc, 1)
      P = LBOUND(ptrc, 2)
      Q = UBOUND(ptrc, 2)

      CALL Sub1(ptrc(:,:,L:U))
      CALL Sub1(ptrc(:,:,P:Q))
      CALL Sub1(ptrc(L:,:,:))
      CALL Sub1(ptrc(L:,L:,L:))
      CALL Sub1(ptrc(L:U,L:U,L:U))
      CALL Sub1(ptrc(LBOUND(ptrc, 1):UBOUND(ptrc, 1),   &
&                    LBOUND(ptrc, 2):UBOUND(ptrc, 2),   &
&                    LBOUND(ptrc, 3):UBOUND(ptrc, 3)))

      CONTAINS

      SUBROUTINE Sub1(Arg)
        INTEGER, TARGET, CONTIGUOUS :: Arg(:,:,:)
        INTEGER, POINTER :: ptr(:,:,:)

        IF (      ASSOCIATED(ptr)     ) STOP 10
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) STOP 11

        ptr => Arg
        IF ( .NOT. ASSOCIATED(ptr)    ) STOP 12
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) STOP 13

      END SUBROUTINE Sub1

END PROGRAM arraySectTar2
