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
!*  the kind type parameters
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


  PROGRAM TypeParam2
  USE M
  IMPLICIT NONE

  TYPE(DT), TARGET   ::  DTV(3,3,3)
  CLASS(DT0), POINTER :: Ptr(:,:,:)
  INTEGER :: S(2)=(/1,2/), I, J

    ALLOCATE(Ptr(2,2,2), SOURCE=DT(IArr=(/1,2/)))

    SELECT TYPE (U => Ptr(Ptr(1,1,1)%IArr,Ptr(1,2,1)%IArr,Ptr(1,2,2)%IArr))
    CLASS DEFAULT
    SELECT TYPE (U => U)

      CLASS IS (DT0)
      CLASS IS (DT)
        IF (SIZE(U(2,2,2)%IArr)   .NE. 2)  STOP 30
        IF (KIND(U(2,2,2)%IArr)   .NE. 8)  STOP 31
        IF (ANY(U(:,:,:)%IArr(1 ) .NE. 1)) STOP 32
        IF (ANY(U(:,:,:)%IArr(2 ) .NE. 2)) STOP 33
        IF (ANY(U(:,:,:)%GetInt(1).NE. 1)) STOP 34
        IF (ANY(U(:,:,:)%GetInt(2).NE. 2)) STOP 35

    END SELECT
    END SELECT


  END


