! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/selectType/Quotes/AssocNameInterface1.f
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
!*  Specific Interface's name
!*
!*  (ICE-298940)
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


  PROGRAM AssocNameInterface1
  USE M
  IMPLICIT NONE

  INTERFACE ITF
    FUNCTION Fun()
      INTEGER Fun
    END FUNCTION
  END INTERFACE

  INTERFACE
    FUNCTION Fun1()
     INTEGER Fun1
    END FUNCTION
  END INTERFACE

  CLASS(DT0(:,8)), POINTER :: PTR(:,:,:)
  INTEGER :: S(2)=(/1,2/), I, J
  TYPE(DT(20,8)) :: U(2,2,2)=DT(20,8)(IARR=-1)

    ALLOCATE(Ptr(2,2,2), SOURCE=DT(20,8)(IArr=(/1_8, 2_8/)))

    SELECT TYPE (ITF => Ptr(Fun1():,Fun1():ITF(),Fun1():(Fun1()+1)))
    CLASS DEFAULT

    SELECT TYPE (Fun1 => ITF )
    CLASS IS (DT0(*,8))
      STOP 20
    CLASS IS (DT(*,8))
        IF (ANY(Fun1(:,:,:)%IArr(1)  .NE. 1)) STOP 22
        IF (ANY(Fun1(:,:,:)%IArr(2)  .NE. 2)) STOP 23
        IF (ANY(Fun1(:,:,:)%GetInt(1).NE. 1)) STOP 24
        IF (ANY(Fun1(:,:,:)%GetInt(2).NE. 2)) STOP 25

        Ptr%IArr(1) = -1
        Ptr%IArr(2) = -2

        IF (SIZE(Fun1(2,2,2)%IArr)   .NE. 2)  STOP 30
        IF (KIND(Fun1(2,2,2)%IArr)   .NE. 8)  STOP 31
        IF (ANY(Fun1(:,:,:)%IArr(1)  .NE. -1)) STOP 32
        IF (ANY(Fun1(:,:,:)%IArr(2)  .NE. -2)) STOP 33
        IF (ANY(Fun1(:,:,:)%GetInt(1).NE. -1)) STOP 34
        IF (ANY(Fun1(:,:,:)%GetInt(2).NE. -2)) STOP 35

    END SELECT
    END SELECT

  END

  FUNCTION Fun1()
  INTEGER Fun1
    Fun1 = 1
  END FUNCTION


  FUNCTION Fun()
  INTEGER Fun
    Fun = 2
  END FUNCTION

