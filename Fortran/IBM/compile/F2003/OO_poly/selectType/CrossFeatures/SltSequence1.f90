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
! %POSTCMD: tcomp SltSequence1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltSequence1
!*
!*  DATE                       : Jan. 28, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
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
!*  Diagnostic : the selector is a var of sequence type
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE  :: DT0
      SEQUENCE
      INTEGER(4)      :: IArr(2)=-1
      CHARACTER(1025) :: CArr(2)="!"
    END TYPE

    TYPE :: DT1
      TYPE(DT0) :: Seq
    END TYPE

    TYPE, EXTENDS(DT1) :: DT
    END TYPE

  END MODULE

  PROGRAM SltSequence1
  USE M
  IMPLICIT NONE
  TYPE (DT0)  :: U(2,2,2)

S1: SELECT TYPE (S2 => U)
    TYPE IS (DT0)
    END SELECT S1

  END



