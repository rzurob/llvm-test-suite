! *********************************************************************
!*  ===================================================================
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
!*  Diagnostic : The selector is a private  component
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE  :: DT0
      INTEGER(4)      :: IArr(2)=-1
      CHARACTER(1025), PRIVATE :: CArr(2)="!"
    END TYPE

    TYPE, ABSTRACT :: DT1
      CLASS(DT0), POINTER, PRIVATE :: Ptr
    END TYPE

    TYPE, EXTENDS(DT1) :: DT
      PRIVATE
    END TYPE

  END MODULE

  PROGRAM SltPrivate
  USE M
  IMPLICIT NONE
  TYPE(DT0), TARGET :: V

  TYPE (DT)  :: U

  CALL Sub(U)

  CONTAINS

  SUBROUTINE Sub(U)
  CLASS(DT1) :: U

  SELECT TYPE(U)
  CLASS IS (DT)
    SELECT TYPE (U => U%DT1%Ptr)
    CLASS IS (DT0)
    END SELECT
  END SELECT

  END SUBROUTINE

  END



