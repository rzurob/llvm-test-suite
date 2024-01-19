! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2011-08-25
!*
!*  PRIMARY FUNCTIONS TESTED   : Copy-in/out for assumed shape arrays
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
      INTEGER, PARAMETER :: N=100, N2=50, s=2

      CONTAINS

      SUBROUTINE Sub(Arg)
        INTEGER, CONTIGUOUS, INTENT(IN) :: Arg(:)
        INTEGER :: I

        IF (       .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 20
        IF ( SIZE(Arg) .NE.              N2 ) ERROR STOP 21
        IF ( ANY(Arg   .NE. [(I, I=1,N,s)]) ) ERROR STOP 22
        CALL SubSub(Arg)
      END SUBROUTINE Sub

      SUBROUTINE SubSub(Arg)
        INTEGER :: Arg(N2), I

        IF ( .NOT. IS_CONTIGUOUS(Arg) ) ERROR STOP 30
        IF ( ANY(Arg       .NE.    [(I, I=1,N,s)]) ) ERROR STOP 31
        IF ( ANY(Swap(Arg) .NE.  [(I, I=99,1,-s)]) ) ERROR STOP 32
        IF ( somme(Arg)       .NE.            2500 ) ERROR STOP 33
        IF ( somme(Swap(Arg)) .NE.            2500 ) ERROR STOP 34

        DO I = 1, N2
          IF ( Arg(I)       .NE.  I*s-1 ) ERROR STOP 35             !<--- need to make sure the elements are right
        ENDDO

      END SUBROUTINE SubSub

      FUNCTION Swap(Arg) RESULT(res)
        INTEGER, CONTIGUOUS, INTENT(IN) :: Arg(:)
        INTEGER, DIMENSION(50) :: res, tmp

        tmp = Arg(N2:1:-1)
        res  = tmp
      END FUNCTION Swap

      INTEGER FUNCTION somme(Arg)
        INTEGER, CONTIGUOUS :: Arg(:)
        INTEGER :: tmp(N2)

        tmp = Arg
        somme = SUM(tmp)
      END FUNCTION somme

END MODULE Mod
PROGRAM CopyInOut3
      USE Mod
      IMPLICIT NONE

      INTEGER :: I
      INTEGER, TARGET  :: I3D(N)
      INTEGER, POINTER :: ptr(:)

      I3D = [(I, I=1,N)]
      ptr => I3D(::s)
      IF ( ANY(ptr       .NE.  [(I, I=1,N,s)]) ) ERROR STOP 10
      CALL Sub(ptr)
      IF ( ANY(ptr       .NE.  [(I, I=1,N,s)]) ) ERROR STOP 11
END PROGRAM CopyInOut3
