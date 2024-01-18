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
! %POSTCMD: tcomp VarDef4.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : VarDef4
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
!*   - do control var
!*    (300332)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM VarDef4
  IMPLICIT NONE

  TYPE :: DT
    INTEGER :: i
  END TYPE

  TYPE(DT) :: T
  PARAMETER (T=DT(0))

  ASSOCIATE ( i => T%i )

    DO i=1, 2
    END DO

  END ASSOCIATE


  END

