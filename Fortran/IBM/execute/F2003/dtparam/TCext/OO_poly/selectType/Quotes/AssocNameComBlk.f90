! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/selectType/Quotes/AssocNameComBlk.f
! opt variations: -qnol -qnodeferredlp

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
!*  Common block name
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE, ABSTRACT :: DT0(N1,K1)    ! (20,8)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: IArr(2)
      CONTAINS
      PROCEDURE, PASS(Obj)   :: GetInt
    END TYPE

    TYPE, EXTENDS(DT0) :: DT    ! (20,8)
    END TYPE

    TYPE(DT(20,8)), TARGET   ::  DTV(3,3,3)

  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0(*,8)), INTENT(IN)    :: Obj
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt
      GetInt = Obj%IArr(Num)
    END FUNCTION

  END MODULE


  PROGRAM AssocNameComBlk
  USE M
  IMPLICIT NONE

  CLASS(DT0(:,8)), POINTER :: PTR(:,:,:)
  INTEGER :: S(2), I, J
  TYPE(DT(20,8)) :: U(2,2,2)=DT(20,8)(IARR=-1)
  COMMON /CB/S,I,J

    ALLOCATE(Ptr(2,2,2), SOURCE=DT(20,8)(IArr=(/1_8, 2_8/)))

    SELECT TYPE (As => Ptr(I:,:J,I:J))
    CLASS DEFAULT

    SELECT TYPE (CB => As )
    CLASS IS (DT0(*,8))
      STOP 20
    CLASS IS (DT(*,8))
        IF (ANY(CB(:,:,:)%IArr(1)  .NE. 1)) ERROR STOP 22
        IF (ANY(CB(:,:,:)%IArr(2)  .NE. 2)) ERROR STOP 23
        IF (ANY(CB(:,:,:)%GetInt(1).NE. 1)) ERROR STOP 24
        IF (ANY(CB(:,:,:)%GetInt(2).NE. 2)) ERROR STOP 25

        Ptr%IArr(1) = -1
        Ptr%IArr(2) = -2

        IF (SIZE(CB(2,2,2)%IArr)   .NE. 2)  ERROR STOP 30
        IF (KIND(CB(2,2,2)%IArr)   .NE. 8)  ERROR STOP 31
        IF (ANY(CB(:,:,:)%IArr(1)  .NE. -1)) ERROR STOP 32
        IF (ANY(CB(:,:,:)%IArr(2)  .NE. -2)) ERROR STOP 33
        IF (ANY(CB(:,:,:)%GetInt(1).NE. -1)) ERROR STOP 34
        IF (ANY(CB(:,:,:)%GetInt(2).NE. -2)) ERROR STOP 35

    END SELECT
    END SELECT


  END

  BLOCK DATA INIT
  INTEGER :: S(2), I, J
  COMMON /CB/S, I, J
  DATA S /1,2/, I /1/, J /2/
  END BLOCK DATA INIT

