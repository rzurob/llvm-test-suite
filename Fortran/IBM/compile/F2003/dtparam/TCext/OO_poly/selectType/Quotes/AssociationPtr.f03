! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/Quotes/AssociationPtr.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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
!*  The pointer attribute
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
      CLASS(DT(:,K1)), POINTER :: Ptr(:,:)=>NULL()
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0(*,8)), INTENT(IN)    :: Obj
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt
      GetInt = Obj%IArr(Num)
    END FUNCTION

  END MODULE


  PROGRAM AssociationPtr
  USE M
  IMPLICIT NONE

  CLASS(DT(:,8)), POINTER :: Ptr(:,:)
  INTEGER :: S(2)=(/1,2/), I=1, J=2
  TYPE(DT(20,8)) :: U(2,2)=DT(20,8)(IARR=-1)

    ALLOCATE(Ptr(2,2), SOURCE=DT(20,8)(IArr=(/1_8, 2_8/)))

     Ptr(1,1)%Ptr => Ptr
     Ptr(1,2)%Ptr => Ptr
     Ptr(2,1)%Ptr => Ptr
     Ptr(2,2)%Ptr => Ptr

S1: SELECT TYPE (S2 => Ptr(1,1)%Ptr(1,1)%Ptr)
    CLASS DEFAULT

S2: SELECT TYPE (Ptr => S2 )
  ! CLASS IS (DT0)
  !   STOP 20
    CLASS IS (DT(*,8))
        IF (ANY(Ptr(:,:)%IArr(1)  .NE. 1)) ERROR STOP 22
        IF (ANY(Ptr(:,:)%IArr(2)  .NE. 2)) ERROR STOP 23
        IF (ANY(Ptr(:,:)%GetInt(1).NE. 1)) ERROR STOP 24
        IF (ANY(Ptr(:,:)%GetInt(2).NE. 2)) ERROR STOP 25

        Ptr%IArr(1) = -1
        Ptr%IArr(2) = -2

        IF (SIZE(Ptr(2,2)%IArr)   .NE. 2)  ERROR STOP 30
        IF (KIND(Ptr(2,2)%IArr)   .NE. 8)  ERROR STOP 31
        IF (ANY(Ptr(:,:)%IArr(1)  .NE. -1)) ERROR STOP 32
        IF (ANY(Ptr(:,:)%IArr(2)  .NE. -2)) ERROR STOP 33
        IF (ANY(Ptr(:,:)%GetInt(1).NE. -1)) ERROR STOP 34
        IF (ANY(Ptr(:,:)%GetInt(2).NE. -2)) ERROR STOP 35

        DEALLOCATE (Ptr) ! test the allocate attribute

    END SELECT S2
    END SELECT S1

    DEALLOCATE (Ptr) ! OK

  END

