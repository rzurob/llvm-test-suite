! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  Misc12.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc12
!*
!*  DATE                       : Nov. 02, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*  Label on named select type  construct
!* ( ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc12
  IMPLICIT NONE

  TYPE :: Base
  END TYPE

  CLASS(Base), ALLOCATABLE :: V

  ALLOCATE(V)

8   A:SELECT TYPE ( V )
11     TYPE IS ( Base )
12     CLASS DEFAULT
10       STOP 32
9   END SELECT A

  DEALLOCATE(V)

  END

