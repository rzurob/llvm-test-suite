! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  Misc5.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc5
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
!*    Array initialization failed when ac control is invloved
!*    (Complaint:  1516-050 (S) Expression or initial value must be evaluated at compile time.)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM Misc5

  TYPE :: Base
    INTEGER :: BaseId = 1
  END TYPE

  TYPE(Base) :: V(3)= (/ (base(i), i=1,3)/)

  IF ( ANY (V%BaseId .NE. (/1,2,3/)) ) STOP 11

  END
