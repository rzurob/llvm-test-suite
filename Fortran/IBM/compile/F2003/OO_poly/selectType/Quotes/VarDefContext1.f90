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
!*  Diagnosis on varriable deinition context
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE, ABSTRACT :: DT0
      INTEGER(8)  :: IArr(2)
      CHARACTER(4) :: C=""
      CONTAINS
      PROCEDURE, PASS(Obj)   :: GetInt
    END TYPE

    TYPE, EXTENDS(DT0) :: DT
      CLASS(DT), POINTER :: Ptr(:,:)=>NULL()
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0), INTENT(IN)    :: Obj
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt
      GetInt = Obj%IArr(Num)
    END FUNCTION

  END MODULE


  PROGRAM  VarDefContext1
  USE M
  IMPLICIT NONE


  CLASS(DT0), POINTER :: Ptr(:,:)
  INTEGER :: S(2)=(/1,2/), I=1, J=2
  TYPE(DT) :: U(2,2)=DT(IARR=-1)

  ALLOCATE(Ptr(2,2), SOURCE=DT(IArr=(/1_8, 2_8/)))

    I = 1
    SELECT TYPE (Ptr => Ptr(S,:))
    CLASS DEFAULT
      STOP 40
    CLASS IS (DT0)
      STOP 41
    CLASS IS (DT)

        IF (ANY(Ptr(:,:)%IArr(1)  .NE. 1)) ERROR STOP 22
        IF (ANY(Ptr(:,:)%IArr(2)  .NE. 2)) ERROR STOP 23
        IF (ANY(Ptr(:,:)%GetInt(1).NE. 1)) ERROR STOP 24
        IF (ANY(Ptr(:,:)%GetInt(2).NE. 2)) ERROR STOP 25

        CALL Sub(Ptr%C)

    END SELECT

  CONTAINS

  SUBROUTINE  Sub(Arg)
    CLASS(*), INTENT(INOUT) ::  Arg(:,:)

    SELECT TYPE (Arg)
    TYPE IS (CHARACTER(*))
      Arg="111"
    CLASS DEFAULT
      STOP 111
    END SELECT

  END SUBROUTINE

  END


