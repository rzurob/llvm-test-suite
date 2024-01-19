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
!*  Interface's name
!*
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

    TYPE(DT), TARGET   ::  DTV(3,3,3)

  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0), INTENT(IN)    :: Obj
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt
      GetInt = Obj%IArr(Num)
    END FUNCTION

  END MODULE


  PROGRAM AssocNameInterface
  USE M
  IMPLICIT NONE

  INTERFACE ITF
    FUNCTION Fun()
      INTEGER Fun
    END FUNCTION
  END INTERFACE

  CLASS(DT0), POINTER :: PTR(:,:,:)
  INTEGER :: S(2)=(/1,2/), I, J
  TYPE(DT) :: U(2,2,2)=DT(IARR=-1)

    ALLOCATE(Ptr(2,2,2), SOURCE=DT(IArr=(/1_8, 2_8/)))

    SELECT TYPE (ITF => Ptr(:,:ITF(),:) )
    CLASS IS (DT0)
      STOP 20
    CLASS IS (DT)
        IF (ANY(ITF(:,:,:)%IArr(1)  .NE. 1)) ERROR STOP 22
        IF (ANY(ITF(:,:,:)%IArr(2)  .NE. 2)) ERROR STOP 23
        IF (ANY(ITF(:,:,:)%GetInt(1).NE. 1)) ERROR STOP 24
        IF (ANY(ITF(:,:,:)%GetInt(2).NE. 2)) ERROR STOP 25

        Ptr%IArr(1) = -1
        Ptr%IArr(2) = -2

        IF (SIZE(ITF(2,2,2)%IArr)   .NE. 2)  ERROR STOP 30
        IF (KIND(ITF(2,2,2)%IArr)   .NE. 8)  ERROR STOP 31
        IF (ANY(ITF(:,:,:)%IArr(1)  .NE. -1)) ERROR STOP 32
        IF (ANY(ITF(:,:,:)%IArr(2)  .NE. -2)) ERROR STOP 33
        IF (ANY(ITF(:,:,:)%GetInt(1).NE. -1)) ERROR STOP 34
        IF (ANY(ITF(:,:,:)%GetInt(2).NE. -2)) ERROR STOP 35

    END SELECT

  END

  FUNCTION Fun()
  INTEGER Fun
    Fun = 2
  END FUNCTION

