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
!*  Type Spec : an abstract type
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE  :: DT0
      INTEGER(2) :: IArr(2)
      CONTAINS
      PROCEDURE, PASS(Obj)   :: GetInt
    END TYPE

    TYPE, EXTENDS(DT0), ABSTRACT :: DT1
    END TYPE

    TYPE, EXTENDS(DT1) :: DT
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0), INTENT(IN)    :: Obj
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt
      GetInt = Obj%IArr(Num)
    END FUNCTION

  END MODULE


  PROGRAM  TypeSpecAbs
  USE M
  IMPLICIT NONE

  CLASS(DT1), POINTER  :: U(:,:,:)

    ALLOCATE(U(2,2,2), SOURCE=DT(IArr=(/1_2, 2_2/)))

S1: SELECT TYPE (S2 => U)
    CLASS DEFAULT

S2: SELECT TYPE (U => S2 )
    TYPE IS (DT1)
      STOP  40
    CLASS IS (DT)
      STOP 30
    END SELECT S2
    END SELECT S1

  END



