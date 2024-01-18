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
! %POSTCMD: tcomp Misc28.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc28
!*
!*  DATE                       : Mar. 09, 2005
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
!*   Diagnosis on elemental subroutine
!*    (comp pass-300954)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  TYPE :: DT
    INTEGER :: Id = 1
  END TYPE

  CLASS(*), POINTER :: T

  CONTAINS

    ELEMENTAL SUBROUTINE Set(Arg)
    CLASS(*), INTENT(IN) :: Arg
    ASSOCIATE( AS => Arg)
      ALLOCATE(T, SOURCE=Arg)
    END ASSOCIATE

  END SUBROUTINE

  END


