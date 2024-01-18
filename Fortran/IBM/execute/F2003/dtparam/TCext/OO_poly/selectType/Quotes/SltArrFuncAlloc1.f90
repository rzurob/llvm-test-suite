! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=none /tstdev/OO_poly/selectType/Quotes/SltArrFuncAlloc1.f
! opt variations: -qnok -qnol -qreuse=base

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltArrFuncAlloc1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltArrFuncAlloc1
!*
!*  DATE                       : Jan. 17, 2005
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
!*   The selector is a function call returing an array of poly allocatable
!*   the function is EOSHIFT
!*    (Complain on external/intrinsic attribute-298359)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


 MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    CONTAINS
      PROCEDURE, NoPASS   :: SetId
      PROCEDURE, NoPASS   :: TEST
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base(N2,K2)    ! (4,20,20,4)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
      INTEGER(K2)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
   !  PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child(N3,K3)    ! (4,20,20,4,20,4)
      INTEGER, KIND :: K3
      INTEGER, LEN  :: N3
      INTEGER(K3)   :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
   !  PROCEDURE, PASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*,*,4,*,4)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*,*,4)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4,*,*,4))  :: Arg(:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetId(Arg)
    CLASS(Base(4,*,*,4))  :: Arg(:,:)
      SELECT TYPE (Arg)
        TYPE IS (Base(4,*,*,4))
          Arg%BaseId =  -Arg%BaseId
        TYPE IS (Child(4,*,*,4,*,4))
          Arg%ChildId =  -Arg%ChildId
      END SELECT
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Child(4,*,*,4,*,4))  :: Arg(:,:)
      Arg%ChildId = -Arg%ChildId
    END SUBROUTINE

    SUBROUTINE TEST(Arg)
    CLASS(*) :: Arg(:,:)

    SELECT TYPE ( As  => EOSHIFT(Arg, 1, Arg(:,2), 2) )
    CLASS DEFAULT

    SELECT TYPE ( As  => EOSHIFT(Arg, 1, Arg(:,1), 2) )
        TYPE IS (Child(4,*,*,4,*,4))

          IF ( ANY (SHAPE(As) .NE. (/3,3/) )) STOP 20
          IF ( ANY (As%Base%GetId() .NE. 1 )) STOP 34
          IF ( ANY (As%GetId()      .NE. 2 )) STOP 35
          IF ( ANY (As%BaseId       .NE. 1 )) STOP 36
          IF ( ANY (As%ChildId      .NE. 2 )) STOP 37

          CALL As%SetId(As)
          CALL As%SetId(As%Base)

          IF ( ANY (As%Base%GetId() .NE. -1 )) STOP 44
          IF ( ANY (As%GetId()      .NE. -2 )) STOP 45
          IF ( ANY (As%BaseId       .NE. -1 )) STOP 46
          IF ( ANY (As%ChildId      .NE. -2 )) STOP 47

       CLASS DEFAULT
          STOP 40
       TYPE is (Base(4,*,*,4))
          STOP 32
       TYPE IS (Zero(4,*))
          STOP 38
      END SELECT

    END SELECT

  END  SUBROUTINE

  END MODULE


  PROGRAM SltArrFuncAlloc1
  USE M
  IMPLICIT NONE
  TYPE(Child(4,20,20,4,20,4)) :: V(3,3)

  CALL V(1,1)%TEST(V)

  END



