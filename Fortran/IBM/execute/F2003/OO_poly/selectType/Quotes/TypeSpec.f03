! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 27, 2005
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
!*  the type spec
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE, ABSTRACT :: DT0
      INTEGER(8) :: IArr(2)
      CONTAINS
      PROCEDURE, PASS(Obj)   :: GetInt
    END TYPE

    TYPE, EXTENDS(DT0) :: DT
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0), INTENT(IN)    :: Obj
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt
      GetInt = Obj%IArr(Num)
    END FUNCTION

  END MODULE


  PROGRAM TypeSpec
  USE M
  IMPLICIT NONE

  TYPE(DT), TARGET   ::  DTV(3,3,3)
  CLASS(*), POINTER :: Ptr(:,:,:)
  INTEGER :: S(2)=(/1,2/), I, J

    ALLOCATE(Ptr(2,2,2), SOURCE=1_8)

    SELECT TYPE (U => Ptr)
    CLASS DEFAULT

    SELECT TYPE (U => U(S,S,:))
      TYPE IS (INTEGER(KIND(DTV(1,1,1)%IArr)))
        PRINT*, "OK"
        IF (ANY(U .NE. 1_8)) ERROR STOP 22
      CLASS IS (DT0)
        STOP 23
      CLASS IS (DT)
        STOP 24

    END SELECT
    END SELECT


  END

