! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-08-20
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : copy-in/out for assumed shape arrays
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
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
PROGRAM CopyInOut1
      IMPLICIT NONE

      INTEGER :: I, J, K, init, I3D(5,5,5)

      I3D = RESHAPE( SOURCE = [(I, I=1,125)], SHAPE = [5,5,5] )

      IF ( IS_CONTIGUOUS(I3D(1,1,::2)) ) ERROR STOP 100

      CALL Sub1(I3D(1,1,::2))
      CALL Sub2(I3D(1,1,::2))
      CALL Sub3(I3D(1,1,::2))

      IF ( ANY(I3D .NE. RESHAPE(SOURCE=[(I, I=1,125)], SHAPE=[5,5,5])) ) ERROR STOP 101

      CONTAINS

      SUBROUTINE Sub1(Arg)
        INTEGER, CONTIGUOUS :: Arg(:)

        IF (          .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 10
        IF ( ANY(Arg .NE. [(I, I = 1,125,50)]) ) ERROR STOP 11

        CALL SubSub1(Arg)
        CALL SubSub2(Arg,size(Arg))
      END SUBROUTINE Sub1

      SUBROUTINE Sub2(Arg)
        INTEGER, CONTIGUOUS, INTENT(IN) :: Arg(:)

        IF (          .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 12
        IF ( ANY(Arg .NE. [(I, I = 1,125,50)]) ) ERROR STOP 13

        CALL SubSub1(Arg)
        CALL SubSub2(Arg,size(Arg))
      END SUBROUTINE Sub2

      SUBROUTINE Sub3(Arg)
        INTEGER :: Arg(:)

        IF ( ANY(Arg .NE. [(I, I = 1,125,50)]) ) ERROR STOP 14

        CALL SubSub1(Arg)
        CALL SubSub2(Arg,size(Arg))
      END SUBROUTINE Sub3

      SUBROUTINE SubSub1(Arg)
        INTEGER :: I, Arg(3)

        IF (          .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 15
        IF ( ANY(Arg .NE. [(I, I = 1,125,50)]) ) ERROR STOP 16

        DO I = 1, 3
             IF ( Arg(I)    .NE.  (50*(I-1)+1) ) ERROR STOP 17
        ENDDO

        CALL SubSubSub(Arg)
      END SUBROUTINE SubSub1

      SUBROUTINE SubSub2(Arg,n)
        INTEGER :: I, n, Arg(n)

        IF (          .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 18
        IF ( ANY(Arg .NE. [(I, I = 1,125,50)]) ) ERROR STOP 19

        DO I = 1, n
             IF ( Arg(I)    .NE.  (50*(I-1)+1) ) ERROR STOP 20
        ENDDO

        CALL SubSubSub(Arg)
      END SUBROUTINE SubSub2

      SUBROUTINE SubSubSub(Arg)
        INTEGER :: I
        INTEGER, CONTIGUOUS :: Arg(:)

        IF (          .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 21
        IF ( ANY(Arg .NE. [(I, I = 1,125,50)]) ) ERROR STOP 22

        DO I = 1, 3
             IF ( Arg(I)    .NE.  (50*(I-1)+1) ) ERROR STOP 23
        ENDDO
      END SUBROUTINE SubSubSub
END PROGRAM CopyInOut1
