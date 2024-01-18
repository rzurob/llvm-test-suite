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
!* (Comp failed-299257)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
    TYPE  :: DT0
      CHARACTER(513) :: C0="0"
      CONTAINS
      PROCEDURE, PASS   :: SetObj
    END TYPE

    TYPE, ABSTRACT, EXTENDS(DT0) :: DT1
      CHARACTER(1025) :: C1="1"
    END TYPE

    TYPE, EXTENDS(DT1) :: DT
      CHARACTER(2049) :: C2="2"
    END TYPE

    TYPE(DT),  SAVE, TARGET,  PROTECTED :: Tar
    CLASS(DT), SAVE, POINTER, PROTECTED :: PPtr

    CONTAINS

    SUBROUTINE SetObj(Arg)
    CLASS(DT0) :: Arg
      Arg%C0 = "SetDT0"
    END SUBROUTINE

    SUBROUTINE SetPtr(Ptr, Tar)
    TYPE(DT), TARGET  :: Tar
    CLASS(DT), POINTER :: Ptr
      Ptr => Tar
    END SUBROUTINE

  END MODULE

  PROGRAM Protected1
  USE M
  IMPLICIT NONE

  CALL SetPtr(PPtr, Tar)

  SELECT TYPE (Ptr => PPtr)
  CLASS IS (DT)

    IF (TRIM(Ptr%C0) .NE. "0") ERROR STOP 31
    IF (TRIM(Ptr%C1) .NE. "1") ERROR STOP 32
    IF (TRIM(Ptr%C2) .NE. "2") ERROR STOP 33

  CLASS DEFAULT
    STOP 40
  END SELECT


  END



