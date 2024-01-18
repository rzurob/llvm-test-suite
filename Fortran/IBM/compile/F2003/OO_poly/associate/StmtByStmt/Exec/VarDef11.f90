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
! %POSTCMD: tcomp VarDef11.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : VarDef11
!*
!*  DATE                       : Feb 22, 2005
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
!*   Variable Definition Context on non variable selector
!*   - INTENT(IN)/INTENT(INOUT)
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM VarDef11
  IMPLICIT NONE

  ASSOCIATE ( As1 => 1, As2 => "ok" )
  ASSOCIATE ( As1 => As1, As2 => As2 )
    CALL Sub(As1, As2)
  END ASSOCIATE
  END ASSOCIATE

  CONTAINS
  SUBROUTINE Sub(Arg1, Arg2)
    INTEGER,      INTENT(IN)    :: Arg1
    CHARACTER(*), INTENT(INOUT) :: Arg2
  END SUBROUTINE

  END

