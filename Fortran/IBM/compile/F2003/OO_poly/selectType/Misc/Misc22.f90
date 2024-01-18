! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:  tcomp Misc22.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc21
!*
!*  DATE                       : Jan. 26, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
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
!*  Intent-in entity with associate construct, No warning
!*  (299198)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Misc22

  I = 0
  CALL S(i)

  PRINT *, i

  CONTAINS

  SUBROUTINE S(Arg)
  INTEGER, INTENT(IN) :: Arg

  ASSOCIATE  (Arg => Arg)
    Arg = 1
  END ASSOCIATE

  END SUBROUTINE

  END



