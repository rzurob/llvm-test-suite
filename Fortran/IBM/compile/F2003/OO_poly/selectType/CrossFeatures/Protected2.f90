! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 02, 2005
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
!* Protected
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
    TYPE  :: DT0
      CHARACTER(513) :: C0="0"
      CONTAINS
      PROCEDURE, PASS   :: SetObj
    END TYPE

    !TYPE, ABSTRACT, EXTENDS(DT0) :: DT1
    TYPE, EXTENDS(DT0) :: DT1
      CHARACTER(1025) :: C1="1"
    END TYPE

    TYPE, EXTENDS(DT1) :: DT
      CHARACTER(2049) :: C2="2"
    END TYPE

    TYPE(DT), SAVE, TARGET, PROTECTED :: V

    CONTAINS

    SUBROUTINE SetObj(Arg)
    CLASS(DT0) :: Arg
      Arg%C0 = "SetDT0"
    END SUBROUTINE

  END MODULE

  PROGRAM Protected2
  USE M
  IMPLICIT NONE
  CLASS(*), POINTER :: Ptr

  Ptr => V

  SELECT TYPE (Ptr)
  CLASS IS (DT)
    Ptr%C1 ="?"
  CLASS DEFAULT
    STOP 40
  END SELECT

  IF (V%C1 .NE. "1" ) STOP 40

  END



