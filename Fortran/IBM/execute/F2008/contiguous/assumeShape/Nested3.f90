! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : Nested subroutine call
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!*    - Inner most subroutine has assumed shape array dummy argument
!*      with CONTIGUOUS attribute
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

      CONTAINS

      SUBROUTINE Sub(Arg)
        INTEGER, CONTIGUOUS :: Arg(:)

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 20
        CALL SubSub(Arg)
      END SUBROUTINE Sub

      SUBROUTINE SubSub(Arg)
        INTEGER :: Arg(100), I

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 30
        IF ( ANY(Arg       .NE.  [(I, I=1,100,1)]) ) ERROR STOP 31
        IF ( ANY(Swap(Arg) .NE. [(I, I=100,1,-1)]) ) ERROR STOP 32
        IF ( somme(Arg)       .NE.            5050 ) ERROR STOP 33
        IF ( somme(Swap(Arg)) .NE.            5050 ) ERROR STOP 34

      END SUBROUTINE SubSub

      FUNCTION Swap(Arg) RESULT(res)
        INTEGER, CONTIGUOUS :: Arg(:)
        INTEGER, DIMENSION(100) :: res, tmp

        tmp = Arg(100:1:-1)
        res  = tmp
      END FUNCTION Swap

      INTEGER FUNCTION somme(Arg)
        INTEGER, CONTIGUOUS :: Arg(:)
        INTEGER :: tmp(100)

        tmp = Arg
        somme = SUM(tmp)
      END FUNCTION somme

END MODULE Mod
PROGRAM Nested3
      USE Mod
      IMPLICIT NONE

      INTEGER :: I
      INTEGER, TARGET  :: I3D(100)
      INTEGER, POINTER :: ptr(:)

      I3D = [(I, I=1,100)]
      ptr => I3D

      CALL Sub(ptr)
      IF ( ANY(ptr       .NE.  [(I, I=1,100,1)]) ) ERROR STOP 10

      ALLOCATE( ptr(100), SOURCE= [(I, I=1,100)] )
      CALL Sub(ptr)
      IF ( ANY(ptr       .NE.  [(I, I=1,100,1)]) ) ERROR STOP 11

END PROGRAM Nested3
