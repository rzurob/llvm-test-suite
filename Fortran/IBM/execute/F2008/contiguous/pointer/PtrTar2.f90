! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : PtrTar2.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Data pointer assingment 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : Function result with CONTIGUOUS attribute
!*
!*  DRIVER STANZA              :
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
PROGRAM PtrTar2
      IMPLICIT NONE

      INTEGER  :: L, U, P, Q
      INTEGER  :: I3D(2,2,2)
      INTEGER, POINTER, CONTIGUOUS  :: ptrc(:,:,:)

      ptrc => foo1(I3D)
      ptrc => foo1(I3D(:,:,:))
      ptrc => foo1(I3D(:,:,1:))
      ptrc => foo1(I3D(1:,1:,:))
      ptrc => foo1(I3D(1:,1:,1:))
      ptrc => foo1(I3D(1:2,1:2,:))
      ptrc => foo1(I3D(1:2,1:2,1:2))
      ptrc => foo1(I3D(:,:,1:0))
      ptrc => foo1(I3D(:,:,1:1))
      ptrc => foo1(I3D(:,:,1:2))
      ptrc => foo1(I3D(:,:,1:2:1))

      L = LBOUND(I3D, 1)
      U = UBOUND(I3D, 1)
      P = LBOUND(I3D, 2)
      Q = UBOUND(I3D, 2)

      ptrc => foo1(I3D(:,:,L:U))
      ptrc => foo1(I3D(:,:,P:Q))
      ptrc => foo1(I3D(L:,:,:))
      ptrc => foo1(I3D(L:,L:,L:))
      ptrc => foo1(I3D(L:U,L:U,L:U))
      ptrc => foo1(I3D(LBOUND(I3D, 1):UBOUND(I3D, 1),   &
&                      LBOUND(I3D, 2):UBOUND(I3D, 2),   &
&                      LBOUND(I3D, 3):UBOUND(I3D, 3)))

      ptrc => foo2(I3D)
      ptrc => foo2(I3D(:,:,:))
      ptrc => foo2(I3D(:,:,1:))
      ptrc => foo2(I3D(1:,1:,:))
      ptrc => foo2(I3D(1:,1:,1:))
      ptrc => foo2(I3D(1:2,1:2,:))
      ptrc => foo2(I3D(1:2,1:2,1:2))
      ptrc => foo2(I3D(:,:,1:0))
      ptrc => foo2(I3D(:,:,1:1))
      ptrc => foo2(I3D(:,:,1:2))
      ptrc => foo2(I3D(:,:,1:2:1))
      ptrc => foo2(I3D(:,:,L:U))
      ptrc => foo2(I3D(:,:,P:Q))
      ptrc => foo2(I3D(L:,:,:))
      ptrc => foo2(I3D(L:,L:,L:))
      ptrc => foo2(I3D(L:U,L:U,L:U))
      ptrc => foo2(I3D(LBOUND(I3D, 1):UBOUND(I3D, 1),   &
&                      LBOUND(I3D, 2):UBOUND(I3D, 2),   &
&                      LBOUND(I3D, 3):UBOUND(I3D, 3)))

      CONTAINS

      FUNCTION foo1(Arg)  
        INTEGER, TARGET :: Arg(:,:,:)
        INTEGER, POINTER, CONTIGUOUS :: foo1(:,:,:)

        IF (     ASSOCIATED(foo1)     ) STOP 10
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) STOP 11

        foo1 => Arg

        IF ( .NOT. ASSOCIATED(foo1)    ) STOP 12
        IF ( .NOT. IS_CONTIGUOUS(foo1) ) STOP 13
      END FUNCTION

      FUNCTION foo2(Arg) RESULT(res) 
        INTEGER, TARGET, CONTIGUOUS  :: Arg(:,:,:)
        INTEGER, POINTER, CONTIGUOUS :: res(:,:,:)

        IF (     ASSOCIATED(res)     ) STOP 10
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) STOP 11

        res => Arg

        IF ( .NOT. ASSOCIATED(res)    ) STOP 12
        IF ( .NOT. IS_CONTIGUOUS(res) ) STOP 13
      END FUNCTION

END PROGRAM PtrTar2
