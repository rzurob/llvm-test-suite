! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/selectType/Quotes/AssocNameConstrName.f
! opt variations: -ql

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
!*  Construct name
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
    END TYPE

    TYPE(DT(8)), TARGET   ::  DTV(3,3,3)

  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0(8)), INTENT(IN)    :: Obj
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt
      GetInt = Obj%IArr(Num)
    END FUNCTION

  END MODULE


  PROGRAM AssocNameConstrName
  USE M
  IMPLICIT NONE

  CLASS(DT0(8)), POINTER :: PTR(:,:,:)
  INTEGER :: S(2), I, J
  TYPE(DT(8)) :: U(2,2,2)=DT(8)(IARR=-1)
  COMMON /CB/S,I,J

    ALLOCATE(Ptr(2,2,2), SOURCE=DT(8)(IArr=(/1_8, 2_8/)))

S1: SELECT TYPE (S1 => Ptr(I:,:J,I:J))
    CLASS DEFAULT

S2: SELECT TYPE (S2 => S1 )
    CLASS IS (DT0(8))
      STOP 20
    CLASS IS (DT(8))
        IF (ANY(S2(:,:,:)%IArr(1)  .NE. 1)) STOP 22
        IF (ANY(S2(:,:,:)%IArr(2)  .NE. 2)) STOP 23
        IF (ANY(S2(:,:,:)%GetInt(1).NE. 1)) STOP 24
        IF (ANY(S2(:,:,:)%GetInt(2).NE. 2)) STOP 25

        Ptr%IArr(1) = -1
        Ptr%IArr(2) = -2

        IF (SIZE(S2(2,2,2)%IArr)   .NE. 2)  STOP 30
        IF (KIND(S2(2,2,2)%IArr)   .NE. 8)  STOP 31
        IF (ANY(S2(:,:,:)%IArr(1)  .NE. -1)) STOP 32
        IF (ANY(S2(:,:,:)%IArr(2)  .NE. -2)) STOP 33
        IF (ANY(S2(:,:,:)%GetInt(1).NE. -1)) STOP 34
        IF (ANY(S2(:,:,:)%GetInt(2).NE. -2)) STOP 35

    END SELECT S2
    END SELECT S1


  END

  BLOCK DATA INIT
  INTEGER :: S(2), I, J
  COMMON /CB/S, I, J
  DATA S /1,2/, I /1/, J /2/
  END BLOCK DATA INIT

