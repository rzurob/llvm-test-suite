! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  Misc10.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc10
!*
!*  DATE                       : Nov. 12, 2004
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
!*    Associate selector is of form : scalar + array
!*    Wrong check on selector shape conformance
!*  " line 47.14: 1516-077 (S) Conformable array is required in this context"
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM Misc10

  ASSOCIATE (as => 1 + (/1,2,3/) )
    IF (Any(As .NE. (/2,3,4/))) STOP 11
  END ASSOCIATE

  END
