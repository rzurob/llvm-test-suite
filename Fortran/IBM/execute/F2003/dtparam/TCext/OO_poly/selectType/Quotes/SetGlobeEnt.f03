! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/selectType/Quotes/SetGlobeEnt.f
! opt variations: -qnol -qnodeferredlp

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
!*  Set globe entities
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

  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0(*,8)), INTENT(IN)    :: Obj
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt
      GetInt = Obj%IArr(Num)
    END FUNCTION

  END MODULE


  PROGRAM SetGlobeEnt
  USE M
  IMPLICIT NONE

  TYPE(DT(20,8)), TARGET   ::  DTV(3,3,3)
  CLASS(DT0(:,8)), POINTER :: PTR(:,:,:)
  INTEGER :: S(2)=(/1,2/), I, J
  TYPE(DT(20,8)) :: U(2,2,2)=DT(20,8)(IARR=-1)

    ALLOCATE(Ptr(2,2,2), SOURCE=DT(20,8)(IArr=(/1_8, 2_8/)))

    ASSOCIATE ( U => U )

    SELECT TYPE (U => Ptr(:,:,:))
    CLASS IS (DT0(*,8))
      STOP 20
    CLASS IS (DT(*,8))
      ASSOCIATE (U => U)

        IF (ANY(U(:,:,:)%IArr(1)  .NE. 1)) ERROR STOP 22
        IF (ANY(U(:,:,:)%IArr(2)  .NE. 2)) ERROR STOP 23
        IF (ANY(U(:,:,:)%GetInt(1).NE. 1)) ERROR STOP 24
        IF (ANY(U(:,:,:)%GetInt(2).NE. 2)) ERROR STOP 25

        Ptr%IArr(1) = -1
        Ptr%IArr(2) = -2

        IF (SIZE(U(2,2,2)%IArr)   .NE. 2)  ERROR STOP 30
        IF (KIND(U(2,2,2)%IArr)   .NE. 8)  ERROR STOP 31
        IF (ANY(U(:,:,:)%IArr(1)  .NE. -1)) ERROR STOP 32
        IF (ANY(U(:,:,:)%IArr(2)  .NE. -2)) ERROR STOP 33
        IF (ANY(U(:,:,:)%GetInt(1).NE. -1)) ERROR STOP 34
        IF (ANY(U(:,:,:)%GetInt(2).NE. -2)) ERROR STOP 35

      END ASSOCIATE
    END SELECT

    END ASSOCIATE

  END

