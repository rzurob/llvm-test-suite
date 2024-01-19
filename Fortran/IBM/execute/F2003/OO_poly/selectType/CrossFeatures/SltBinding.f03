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
!*  The selector is a binding func call
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE  :: DT0
      INTEGER(4)      :: IArr(2)=-1
      CHARACTER(1025) :: CArr(2)="!"
    END TYPE

    TYPE, ABSTRACT, EXTENDS(DT0) :: DT1
      CLASS(DT0), POINTER, PRIVATE :: Ptr
      CONTAINS
      PROCEDURE, NoPASS   :: GetObj
    END TYPE

    TYPE, EXTENDS(DT1) :: DT
      PRIVATE
    END TYPE

    CONTAINS

    FUNCTION GetObj(Arg)
    CLASS(*),TARGET, INTENT(IN) :: Arg
    CLASS(*), POINTER  :: GetObj
      GetObj => Arg
    END FUNCTION

  END MODULE

  PROGRAM SltBinding
  USE M
  IMPLICIT NONE
  TYPE(DT), TARGET :: V

  CALL Sub(V)

  CONTAINS

  SUBROUTINE Sub(U)
  CLASS(DT), TARGET :: U

  SELECT TYPE(U => U%GetObj(U))
  CLASS IS (DT)
    SELECT TYPE (U => U%GetObj(U%DT1%DT0))
    CLASS IS (DT)
      STOP 20
    CLASS IS (DT0)
      IF (ANY(U%IArr .NE. -1)) ERROR STOP 21
      IF (TRIM(U%CArr(1)) .NE. "!") ERROR STOP 22
      IF (TRIM(U%CArr(2)) .NE. "!") ERROR STOP 23
    CLASS DEFAULT
      STOP 30
    END SELECT
  CLASS DEFAULT
    STOP 31
  END SELECT

  END SUBROUTINE

  END
