! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: InTypeIs2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InTypeIs2
!*
!*  DATE                       : Jan. 24, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Within TYPE IS, test charater(*)
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InTypeIs2
  IMPLICIT NONE
  CHARACTER(4) :: V(3,3)="0000"

  V(1,1)="1234"
  V(1,3)="1234"
  V(3,1)="1234"
  V(3,3)="1234"

  CALL Sub(V(1:3:2,1:3:2))

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg(2:3,3:4)

    SELECT TYPE (Arg=>Arg(2, 3))
    TYPE IS (CHARACTER(*))
      IF ( LEN(Arg) .NE. 4) STOP 20
      IF ( Arg .NE. "1234") STOP 20

    CLASS DEFAULT
      STOP 21
    END SELECT

    SELECT TYPE (Arg)
    TYPE IS (CHARACTER(*))
      IF ( SIZE(Arg) .NE. 4) STOP 20
      IF ( LEN(Arg)  .NE. 4) STOP 20
      IF ( Any(Arg   .NE. "1234")) STOP 20
    CLASS DEFAULT
      STOP 41
    END SELECT

  END SUBROUTINE

  END



