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
! %POSTCMD: tcomp Misc14.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  Misc15.f
!*
!*  DATE                       : Jun. 09, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!* Volatile - no intent
!*
!*  (304980)
!*
!234567890123456789012345678901234567890123456789012345678901234567890





  SUBROUTINE Sub(Arg)
  INTEGER, VOLATILE, INTENT(IN) :: arg
  END SUBROUTINE

  PROGRAM Misc14
  END

