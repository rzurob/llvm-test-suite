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
! %POSTCMD: tcomp VarDef61.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : VarDef61
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
!*   - Read
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM VarDef61
  IMPLICIT NONE
  INTEGER :: Fun , i
  Fun(i)=i

  ASSOCIATE ( As => Fun(1) )

    READ *, As

  END ASSOCIATE

  END

