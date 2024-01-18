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
! %POSTCMD: tcomp AssocNameScope.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : AssocNameScope.f
!*
!*  DATE                       : Mar. 04, 2005
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
!*  The associate name's scope
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM AssocNameScope
  IMPLICIT NONE

  ASSOCIATE ( A =>1)
    ASSOCIATE ( B => A )
      PRINT *, F()
    END ASSOCIATE
    PRINT *, B
  END ASSOCIATE

  PRINT *, A

  CONTAINS
  FUNCTION F()
  INTEGER :: F
     F = A
  END FUNCTION

  END
