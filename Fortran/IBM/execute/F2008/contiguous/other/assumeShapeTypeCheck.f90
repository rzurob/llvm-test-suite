! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2010-10-25
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*
!*  KEYWORD(S)                 :
!*  (S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
MODULE Mod1
      IMPLICIT NONE

      CONTAINS

      SUBROUTINE iSub(Arg)
        INTEGER, CONTIGUOUS :: Arg(:)

        Arg = 1
        IF ( .NOT. IS_CONTIGUOUS(Arg) )       ERROR STOP 30
      END SUBROUTINE iSub

      SUBROUTINE rSub(Arg)
        REAL, CONTIGUOUS ::  Arg(:)

        Arg = 1.0
        IF ( .NOT. IS_CONTIGUOUS(Arg) )       ERROR STOP 31
      END SUBROUTINE rSub

      SUBROUTINE lSub(Arg)
        LOGICAL, CONTIGUOUS :: Arg(:)

        Arg = .True.
        IF ( .NOT. IS_CONTIGUOUS(Arg) )       ERROR STOP 32
      END SUBROUTINE lSub

      SUBROUTINE zSub(Arg)
        COMPLEX, CONTIGUOUS :: Arg(:)

        Arg = (1.0,1.0)
        IF ( .NOT. IS_CONTIGUOUS(Arg) )       ERROR STOP 33
      END SUBROUTINE zSub

      SUBROUTINE cSub(Arg)
        CHARACTER(10), CONTIGUOUS :: Arg(:)

        Arg = 'XlFtest'
        IF ( .NOT. IS_CONTIGUOUS(Arg) )       ERROR STOP 34
      END SUBROUTINE cSub

      SUBROUTINE dSub(Arg)
        DOUBLE PRECISION, CONTIGUOUS :: Arg(:)

        Arg = 1.0d0
        IF ( .NOT. IS_CONTIGUOUS(Arg) )       ERROR STOP 35
      END SUBROUTINE dSub
END MODULE Mod1
PROGRAM assumeShapeTypeCheck
      USE Mod1

      REAL               :: rt(10)
      INTEGER            :: it(100)
      LOGICAL            :: lt(2)
      COMPLEX            :: zt(10)
      CHARACTER(10)      :: ct(10)
      DOUBLE PRECISION   :: dt(10)

      IF ( .NOT. IS_CONTIGUOUS(it) ) ERROR STOP 10
      CALL iSub(it)
      IF ( .NOT. IS_CONTIGUOUS(it) ) ERROR STOP 11

      IF ( .NOT. IS_CONTIGUOUS(rt) ) ERROR STOP 12
      CALL rSub(rt)
      IF ( .NOT. IS_CONTIGUOUS(rt) ) ERROR STOP 13

      IF ( .NOT. IS_CONTIGUOUS(lt) ) ERROR STOP 14
      CALL lSub(lt)
      IF ( .NOT. IS_CONTIGUOUS(lt) ) ERROR STOP 15

      IF ( .NOT. IS_CONTIGUOUS(zt) ) ERROR STOP 16
      CALL zSub(zt)
      IF ( .NOT. IS_CONTIGUOUS(zt) ) ERROR STOP 17

      IF ( .NOT. IS_CONTIGUOUS(ct) ) ERROR STOP 18
      CALL cSub(ct)
      IF ( .NOT. IS_CONTIGUOUS(ct) ) ERROR STOP 19

      IF ( .NOT. IS_CONTIGUOUS(dt) ) ERROR STOP 20
      CALL dSub(dt)
      IF ( .NOT. IS_CONTIGUOUS(dt) ) ERROR STOP 21
END PROGRAM assumeShapeTypeCheck
