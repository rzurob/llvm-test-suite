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
! %POSTCMD:  tcomp Misc25.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc25
!*
!*  DATE                       : Feb 03, 2005
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
!*  Check on definablity
!*  (ICE-299346)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Misc25

  CALL Sub("123 ")

  CONTAINS

  SUBROUTINE Sub( V)
  CHARACTER(*), INTENT(IN)  :: V
  ASSOCIATE ( V => V)
    V = "Bad!"
    PRINT *, V
  END ASSOCIATE

  END SUBROUTINE


  END




