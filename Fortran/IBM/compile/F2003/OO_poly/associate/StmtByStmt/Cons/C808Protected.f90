! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp C808Protected.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C808Protected
!*
!*  DATE                       : Oct. 20, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : Selector is a constant
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
!*    The selector is a protected entity
!*    (Pass com!)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    INTEGER, PROTECTED :: P = 9
  END MODULE

  PROGRAM C808Protected
    USE M
    IMPLICIT NONE

    PRINT*, P
    ASSOCIATE ( As => P )
      As = As + 1
      print*, As
    END ASSOCIATE

  END
