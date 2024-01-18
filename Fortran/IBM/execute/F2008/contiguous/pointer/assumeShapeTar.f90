! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : assumeShapeTar.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Data pointer assingment 
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : - Target is assumed shape dummy argument 
!*                                 with contiguous attribute
!*                      
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
PROGRAM assumeShapeTar
      IMPLICIT NONE

      INTEGER  :: I, J, K, L, U, P, Q
      INTEGER  :: I3D(5,5,5)
      INTEGER, POINTER, CONTIGUOUS  :: ptrc(:,:,:)

      I3D = RESHAPE( SOURCE = [(I, I=1,125)], SHAPE = [5,5,5] )

      CALL Sub1(I3D)
      CALL Sub1(I3D(:,:,:))
      CALL Sub1(I3D(:,:,1:))
      CALL Sub1(I3D(1:,1:,:))
      CALL Sub1(I3D(1:,1:,1:))
      CALL Sub1(I3D(1:5,1:5,:))
      CALL Sub1(I3D(1:5,1:5,1:5))
      CALL Sub1(I3D(:,:,1:5))
      CALL Sub1(I3D(:,:,1:5:1))

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
      ALLOCATE( ptrc(5,5,5), SOURCE = I3D )

      CALL Sub1(ptrc)
      CALL Sub1(ptrc(:,:,:))
      CALL Sub1(ptrc(:,:,1:))
      CALL Sub1(ptrc(1:,1:,:))
      CALL Sub1(ptrc(1:,1:,1:))
      CALL Sub1(ptrc(1:5,1:5,:))
      CALL Sub1(ptrc(1:5,1:5,1:5))
      CALL Sub1(ptrc(:,:,1:5))
      CALL Sub1(ptrc(:,:,1:5:1))

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


      CALL Sub2(ptrc)

      CONTAINS

      SUBROUTINE Sub1(Arg)
        INTEGER, TARGET, CONTIGUOUS  :: Arg(:,:,:)
        INTEGER, POINTER, CONTIGUOUS :: ptr(:,:,:)

        IF (      ASSOCIATED(ptr)     ) ERROR STOP 10
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 11
        IF ( ANY(Arg .NE. RESHAPE(SOURCE = [(I, I=1,125)], SHAPE = [5,5,5])) ) ERROR STOP 12

        ptr => Arg
        IF ( .NOT. ASSOCIATED(ptr)    ) ERROR STOP 13
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 14
        IF ( ANY(ptr .NE. RESHAPE(SOURCE = [(I, I=1,125)], SHAPE = [5,5,5])) ) ERROR STOP 15

      END SUBROUTINE Sub1

      SUBROUTINE Sub2(Arg)  
        INTEGER, POINTER, CONTIGUOUS :: Arg(:,:,:)
        INTEGER, POINTER, CONTIGUOUS :: ptr(:,:,:)

        IF (       ASSOCIATED(ptr)    ) ERROR STOP 20
        IF ( .NOT. ASSOCIATED(Arg)    ) ERROR STOP 21
        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 22
        IF ( ANY(Arg .NE. RESHAPE(SOURCE = [(I, I=1,125)], SHAPE = [5,5,5])) ) ERROR STOP 23

        ptr=>Arg

        IF ( .NOT. ASSOCIATED(ptr)    ) ERROR STOP 24
        IF ( .NOT. IS_CONTIGUOUS(ptr) ) ERROR STOP 25
        IF ( ANY(ptr .NE. RESHAPE(SOURCE = [(I, I=1,125)], SHAPE = [5,5,5])) ) ERROR STOP 26

      END SUBROUTINE Sub2
END PROGRAM assumeShapeTar
