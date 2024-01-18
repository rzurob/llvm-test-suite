! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Misc26.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc26
!*
!*  DATE                       : Feb. 16, 2005
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
!*
!*  Dev complains the init expre
!*  (300174)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc26


  TYPE :: Base
    INTEGER :: BaseId = 1
  END TYPE

  integer :: i

  TYPE(Base)  :: V(3) = (/(Base(i), i =1, 3)/)

  IF (ANY(V%BaseID  .NE. (/1,2,3/)) ) STOP 11

  END



