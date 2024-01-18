! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/selectType/Quotes/ScopeAssocName.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  ScopeAssocName.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ScopeAssocName
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
!*  The scope of associate name
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


  PROGRAM ScopeAssocName
  USE M
  IMPLICIT NONE

  TYPE(DT(20,8)), TARGET   ::  DTV(3,3,3)
  CLASS(DT0(:,8)), POINTER :: PTR(:,:,:)
  INTEGER :: S(2)=(/1,2/), I, J
  TYPE(DT(20,8)) :: U(2,2,2)=DT(20,8)(IARR=-1)

    ALLOCATE(Ptr(2,2,2), SOURCE=DT(20,8)(IArr=(/1_8, 2_8/)))

    SELECT TYPE (U => Ptr(:,:,:))
    CLASS IS (DT0(*,8))
      STOP 20
    CLASS IS (DT(*,8))

        IF (SIZE(U(2,2,2)%IArr)   .NE. 2)  STOP 30
        IF (KIND(U(2,2,2)%IArr)   .NE. 8)  STOP 31
        IF (ANY(U(:,:,:)%IArr(1 ) .NE. 1)) STOP 32
        IF (ANY(U(:,:,:)%IArr(2 ) .NE. 2)) STOP 33
        IF (ANY(U(:,:,:)%GetInt(1).NE. 1)) STOP 34
        IF (ANY(U(:,:,:)%GetInt(2).NE. 2)) STOP 35
        U%IArr(1) = 0
        U%IArr(2) = 0

    END SELECT

    IF (ANY(U%IArr(1) .NE. -1)) STOP 40
    IF (ANY(U%IArr(2) .NE. -1)) STOP 41

  END


