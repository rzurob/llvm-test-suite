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
! %POSTCMD: tcomp Misc7.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc7
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
!*  The entity denotation is internal
!*  "../associate/StmtByStmt/Exec/Misc7.f", 1514-540 (S)
!*  The entity __m_NMOD_wrong    <-----
!*  declared with the CLASS keyword must be a dummy argument,
!*  or have the ALLOCATABLE or POINTER attribute."
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
    END TYPE

    CONTAINS

    FUNCTION Wrong()
    CLASS(Base)  :: Wrong
    END FUNCTION

  END MODULE

  END

