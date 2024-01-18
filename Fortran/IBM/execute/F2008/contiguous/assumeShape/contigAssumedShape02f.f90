! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : CopyInOut2.f
!*
!*  PROGRAMMER                 : Dorra Bouchiha 
!*  DATE                       : 2011-08-20
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : copy-in/out for assumed shape arrays
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : 
!*       
!*                      
!*    - Non contigusous actual argument is a pointer 
!*    - Outer most subroutine has assumed shape array dummy argument with contiguous attribute
!*    - Second subroutine has explit shape array dummy argument 
!*    - Inner most subroutine has assumed shape array dummy argument with contiguous attribute 
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
PROGRAM CopyInOut2
      IMPLICIT NONE

      INTEGER :: I
      INTEGER, TARGET  :: tgt(5,5,5), test(1,3,3)
      INTEGER, POINTER :: ptr(:,:,:)

      test = RESHAPE(SOURCE=[1,11,21,51,61,71,101,111,121], SHAPE=[1,3,3])

      tgt = RESHAPE( SOURCE = [(I, I=1,125)], SHAPE = [5,5,5] )                                      !<--- shape (5,5,5)

      ptr => tgt(::5,::2,::2)                                                                        !<--- shape (1,3,3)
      IF ( ANY(tgt(::5,::2,::2) .NE. test) ) ERROR STOP 101
      IF ( ANY(ptr              .NE. test) ) ERROR STOP 102

      IF ( IS_CONTIGUOUS(ptr) ) ERROR STOP 103

      CALL Sub1(ptr)
      CALL Sub2(ptr)
      CALL Sub3(ptr)

      IF ( ANY(ptr .NE. test) ) ERROR STOP 104

      CONTAINS

      SUBROUTINE Sub1(Arg)           
        INTEGER, CONTIGUOUS :: Arg(:,:,:)

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 10
        IF ( ANY(Arg       .NE. test) ) ERROR STOP 11

        CALL SubSub1(Arg(1,::2,:))                                                                    !<--- shape (2,3)
        CALL SubSub2(Arg(1,::2,:),UBOUND(Arg(1,::2,:),1), UBOUND(Arg(1,::2,:),2))
      END SUBROUTINE Sub1
      
      SUBROUTINE Sub2(Arg)
        INTEGER, CONTIGUOUS, INTENT(INOUT) :: Arg(:,:,:)

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 12
        IF ( ANY(Arg       .NE. test) ) ERROR STOP 13

        CALL SubSub1(Arg(1,::2,:))
        CALL SubSub2(Arg(1,::2,:),UBOUND(Arg(1,::2,:),1), UBOUND(Arg(1,::2,:),2))
      END SUBROUTINE Sub2

      SUBROUTINE Sub3(Arg)           
        INTEGER :: Arg(:,:,:)

        IF ( ANY(Arg       .NE. test) ) ERROR STOP 14

        CALL SubSub1(Arg(1,::2,:))
        CALL SubSub2(Arg(1,::2,:),UBOUND(Arg(1,::2,:),1), UBOUND(Arg(1,::2,:),2))
      END SUBROUTINE Sub3

      SUBROUTINE SubSub1(Arg)
        INTEGER :: I, J, Arg(2,3)

        IF (           .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 15
        IF ( ANY(Arg .NE. RESHAPE(SOURCE=[1,21,51,71,101,121], SHAPE=[2,3])) ) ERROR STOP 16

        DO I = 1, 2
           DO J = 1, 3
             IF ( Arg(I,J)    .NE.  1+(20*(I-1)+50*(J-1)) ) ERROR STOP 17 
           ENDDO
        ENDDO
        CALL SubSubSub(Arg(2,::2))                                                                !<---- shape (2)
      END SUBROUTINE SubSub1

      SUBROUTINE SubSub2(Arg,n,m)
        INTEGER :: I, J, n, m, Arg(n,m)

        IF (          .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 18
        IF ( ANY(Arg .NE. RESHAPE(SOURCE=[1,21,51,71,101,121], SHAPE=[2,3])) ) ERROR STOP 19

        DO I = 1, n
           DO J = 1, m
             IF ( Arg(I,J)    .NE.  1+(20*(I-1)+50*(J-1)) ) ERROR STOP 20
           ENDDO
        ENDDO

        CALL SubSubSub(Arg(2,::2))
      END SUBROUTINE SubSub2

      SUBROUTINE SubSubSub(Arg)
        INTEGER :: I
        INTEGER, CONTIGUOUS :: Arg(:)

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 21
        IF ( ANY(Arg   .NE. [21,121]) ) ERROR STOP 22
        IF ( Arg(1)    .NE.        21 ) ERROR STOP 23
        IF ( Arg(2)    .NE.       121 ) ERROR STOP 24
      END SUBROUTINE SubSubSub
END PROGRAM CopyInOut2
