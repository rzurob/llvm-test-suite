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
!*  The associating entity outside of a select type construct shall not exist
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


  PROGRAM AssociationNonExist
  USE M
  IMPLICIT NONE

  CLASS(DT), POINTER :: Ptr(:,:),S1(:,:),S2(:,:)
  INTEGER :: S(2)=(/1,2/), I=1, J=2
  TYPE(DT) :: U(2,2)=DT(IARR=-1)

    ALLOCATE(Ptr(2,2), SOURCE=DT(IArr=(/1_8, 2_8/)))
    ALLOCATE(S1(2,2), SOURCE=DT(IArr=(/3_8, 4_8/)))
    ALLOCATE(S2(2,2), SOURCE=DT(IArr=(/5_8, 6_8/)))

     Ptr(1,1)%Ptr => Ptr
     Ptr(1,2)%Ptr => Ptr
     Ptr(2,1)%Ptr => Ptr
     Ptr(2,2)%Ptr => Ptr

    SELECT TYPE (S2 => Ptr(1,1)%Ptr(1,1)%Ptr)
    CLASS DEFAULT

    SELECT TYPE (S1 => S2 )
  ! CLASS IS (DT0)
  !   STOP 20
    CLASS IS (DT)
        IF (ANY(S1(:,:)%IArr(1)  .NE. 1)) ERROR STOP 22
        IF (ANY(S1(:,:)%IArr(2)  .NE. 2)) ERROR STOP 23
        IF (ANY(S1(:,:)%GetInt(1).NE. 1)) ERROR STOP 24
        IF (ANY(S1(:,:)%GetInt(2).NE. 2)) ERROR STOP 25

        S1%IArr(1) = -1 !Bad
        S1%IArr(2) = -2 !Bad

        IF (SIZE(S1(2,2)%IArr)   .NE. 2)  ERROR STOP 30
        IF (KIND(S1(2,2)%IArr)   .NE. 8)  ERROR STOP 31
        IF (ANY(S1(:,:)%IArr(1)  .NE. -1)) ERROR STOP 32
        IF (ANY(S1(:,:)%IArr(2)  .NE. -2)) ERROR STOP 33
        IF (ANY(S1(:,:)%GetInt(1).NE. -1)) ERROR STOP 34
        IF (ANY(S1(:,:)%GetInt(2).NE. -2)) ERROR STOP 35


    END SELECT

    IF(ANY( S1%IArr(1) .NE. 3_8 )) ERROR STOP 40
    IF(ANY( S1%IArr(2) .NE. 4_8 )) ERROR STOP 41

    END SELECT

    IF(ANY( S2%IArr(1) .NE. 5_8 )) ERROR STOP 50
    IF(ANY( S2%IArr(2) .NE. 6_8 )) ERROR STOP 51

  END


