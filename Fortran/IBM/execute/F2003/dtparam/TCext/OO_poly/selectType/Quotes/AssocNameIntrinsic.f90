! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_poly/selectType/Quotes/AssocNameIntrinsic.f
! opt variations: -qnok -qnol -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  AssocNameIntrinsic.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : AssocNameIntrinsic
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
!*  Intrinsic type name
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

    TYPE, EXTENDS(DT0) :: DT(K2,N2)    ! (20,8,4,20)
        INTEGER, KIND :: K2
        INTEGER, LEN  :: N2
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetInt(Num, Obj)
    CLASS(DT0(*,8)), INTENT(IN)    :: Obj
    INTEGER, INTENT(IN)      :: Num
    INTEGER(KIND(Obj%IArr))   :: GetInt
      GetInt = Obj%IArr(Num)
    END FUNCTION

  END MODULE


  PROGRAM AssocNameIntrinsic
  USE M
  IMPLICIT NONE

  TYPE(DT(20,8,4,20)), TARGET   ::  DTV(3,3,3)
  CLASS(DT0(:,8)), POINTER :: PTR(:,:,:)
  INTEGER :: S(2)=(/1,2/), I, J
  TYPE(DT(20,8,4,20)) :: U(2,2,2)=DT(20,8,4,20)(IARR=-1)

    ALLOCATE(Ptr(2,2,2), SOURCE=DT(20,8,4,20)(IArr=(/1_8, 2_8/)))

    ASSOCIATE ( REAL => U )

    SELECT TYPE (REAL => Ptr(:,:,:))
    CLASS IS (DT0(*,8))
      STOP 20
    CLASS IS (DT(*,8,4,*))
      ASSOCIATE (INTEGER => REAL)

        IF (ANY(INTEGER(:,:,:)%IArr(1)  .NE. 1)) STOP 22
        IF (ANY(INTEGER(:,:,:)%IArr(2)  .NE. 2)) STOP 23
        IF (ANY(INTEGER(:,:,:)%GetInt(1).NE. 1)) STOP 24
        IF (ANY(INTEGER(:,:,:)%GetInt(2).NE. 2)) STOP 25

        Ptr%IArr(1) = -1
        Ptr%IArr(2) = -2

        IF (SIZE(INTEGER(2,2,2)%IArr)   .NE. 2)  STOP 30
        IF (KIND(INTEGER(2,2,2)%IArr)   .NE. 8)  STOP 31
        IF (ANY(INTEGER(:,:,:)%IArr(1)  .NE. -1)) STOP 32
        IF (ANY(INTEGER(:,:,:)%IArr(2)  .NE. -2)) STOP 33
        IF (ANY(INTEGER(:,:,:)%GetInt(1).NE. -1)) STOP 34
        IF (ANY(INTEGER(:,:,:)%GetInt(2).NE. -2)) STOP 35

        DTV = INTEGER(2,2,2)
        PRINT*, DTV
      END ASSOCIATE
    END SELECT

    END ASSOCIATE

  END


