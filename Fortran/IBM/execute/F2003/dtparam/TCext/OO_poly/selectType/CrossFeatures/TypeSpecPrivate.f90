! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/CrossFeatures/TypeSpecPrivate.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

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
!*  Type Spec : a type with private components
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE  :: DT0(N1,K1)    ! (20,2)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: IArr(2)
      CONTAINS
      PROCEDURE, PASS(Obj)  :: GetInt
    END TYPE

    TYPE, EXTENDS(DT0), ABSTRACT :: DT1(K2)    ! (20,2,4)
      INTEGER, KIND :: K2
      PRIVATE
      INTEGER(K2)   :: I=10
    END TYPE

    TYPE, EXTENDS(DT1) :: DT    ! (20,2,4)
      PRIVATE
      INTEGER(K2), PRIVATE :: J=11
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0(*,2)), INTENT(IN)    :: Obj
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt
      GetInt = Obj%IArr(Num)
    END FUNCTION

  END MODULE


  PROGRAM  TypeSpecPrivate
  USE M
  IMPLICIT NONE

  CLASS(DT0(:,2)), POINTER  :: U(:,:,:)

    ALLOCATE(U(2,2,2), SOURCE=DT(20,2,4)(DT0=DT0(20,2)(IArr=(/1_2, 2_2/))))

S1: SELECT TYPE (S2 => U)
    CLASS DEFAULT

S2: SELECT TYPE (U => S2 )
    CLASS IS (DT0(*,2))
      STOP 20
    CLASS IS (DT1(*,2,4))
        IF (ANY(U(:,:,:)%IArr(1)  .NE. 1)) STOP 22
        IF (ANY(U(:,:,:)%IArr(2)  .NE. 2)) STOP 23
        IF (ANY(U(:,:,:)%GetInt(1).NE. 1)) STOP 24
        IF (ANY(U(:,:,:)%GetInt(2).NE. 2)) STOP 25

        U%IArr(1) = -1
        U%IArr(2) = -2

        IF (SIZE(U(2,2,2)%IArr)   .NE. 2)  STOP 30
        IF (KIND(U(2,2,2)%IArr)   .NE. 2)  STOP 31
        IF (ANY(U(:,:,:)%IArr(1)  .NE. -1)) STOP 32
        IF (ANY(U(:,:,:)%IArr(2)  .NE. -2)) STOP 33
        IF (ANY(U(:,:,:)%GetInt(1).NE. -1)) STOP 34
        IF (ANY(U(:,:,:)%GetInt(2).NE. -2)) STOP 35

    END SELECT S2
    END SELECT S1

  END



