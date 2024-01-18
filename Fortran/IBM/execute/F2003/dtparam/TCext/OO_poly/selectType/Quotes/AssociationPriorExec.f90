! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/selectType/Quotes/AssociationPriorExec.f
! opt variations: -ql -qdefaultpv -qreuse=none

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
!*  The association is established prior to execution of
!*  the block
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE, ABSTRACT :: DT0(K1)    ! (8)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: IArr(2)
      CONTAINS
      PROCEDURE, PASS(Obj)   :: GetInt
    END TYPE

    TYPE, EXTENDS(DT0) :: DT    ! (8)
      CLASS(DT(K1)), POINTER :: Ptr(:,:)=>NULL()
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0(8)), INTENT(IN)    :: Obj
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt
      GetInt = Obj%IArr(Num)
    END FUNCTION

  END MODULE


  PROGRAM AssociationPriorExec
  USE M
  IMPLICIT NONE

  CLASS(DT0(8)), POINTER :: Ptr(:,:),S1(:,:),S2(:,:)
  INTEGER :: S(2)=(/1,2/), I=1, J=2
  TYPE(DT(8)) :: U(2,2)=DT(8)(IARR=-1)

    ALLOCATE(Ptr(2,2), SOURCE=DT(8)(IArr=(/1_8, 2_8/)))
    ALLOCATE(S1(2,2), SOURCE=DT(8)(IArr=(/3_8, 4_8/)))
    ALLOCATE(S2(2,2), SOURCE=DT(8)(IArr=(/5_8, 6_8/)))

    I = 1
    SELECT TYPE (Ptr => Ptr(I,:))
    CLASS DEFAULT
      STOP 40
    CLASS IS (DT0(8))
      STOP 41
    CLASS IS (DT(8))
        IF (ANY(Ptr(:)%IArr(1)  .NE. 1)) STOP 22
        IF (ANY(Ptr(:)%IArr(2)  .NE. 2)) STOP 23
        IF (ANY(Ptr(:)%GetInt(1).NE. 1)) STOP 24
        IF (ANY(Ptr(:)%GetInt(2).NE. 2)) STOP 25

        Ptr%IArr(1) = -1
        Ptr%IArr(2) = -2

        IF (SIZE(Ptr(2)%IArr)   .NE. 2)  STOP 30
        IF (KIND(Ptr(2)%IArr)   .NE. 8)  STOP 31
        IF (ANY(Ptr(:)%IArr(1)  .NE. -1)) STOP 32
        IF (ANY(Ptr(:)%IArr(2)  .NE. -2)) STOP 33
        IF (ANY(Ptr(:)%GetInt(1).NE. -1)) STOP 34
        IF (ANY(Ptr(:)%GetInt(2).NE. -2)) STOP 35

        I = 2

        IF (ANY(Ptr(:)%IArr(1)  .NE. -1)) STOP 52
        IF (ANY(Ptr(:)%IArr(2)  .NE. -2)) STOP 53
        IF (ANY(Ptr(:)%GetInt(1).NE. -1)) STOP 54
        IF (ANY(Ptr(:)%GetInt(2).NE. -2)) STOP 55

    END SELECT

        IF (ANY(Ptr(I,:)%IArr(1)  .NE. 1)) STOP 62
        IF (ANY(Ptr(I,:)%IArr(2)  .NE. 2)) STOP 63
        IF (ANY(Ptr(I,:)%GetInt(1).NE. 1)) STOP 64
        IF (ANY(Ptr(I,:)%GetInt(2).NE. 2)) STOP 65

  END


