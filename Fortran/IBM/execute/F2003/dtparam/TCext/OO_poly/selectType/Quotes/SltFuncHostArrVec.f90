! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=none /tstdev/OO_poly/selectType/Quotes/SltFuncHostArrVec.f
! opt variations: -qnok -qnol -qreuse=base

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 21, 2005
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
!*   The selector is a function call with a host associate name associating to
!*   an array with a vector subscript
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    CONTAINS
      PROCEDURE, NoPASS   :: Called
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base(N2,K2)    ! (4,20,20,4)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
      INTEGER(K2)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child(N3,K3)    ! (4,20,20,4,20,4)
      INTEGER, KIND :: K3
      INTEGER, LEN  :: N3
      INTEGER(K3)   :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, NoPASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    FUNCTION Called()
    LOGICAL :: Called
      Called =.true.
    END FUNCTION

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
    CLASS(Base(4,*,*,4)), INTENT(INOUT) :: Arg(:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base(4,*,*,4)), INTENT(INOUT)  :: Arg(:,:)
      SELECT TYPE(Arg)
        TYPE IS (Child(4,*,*,4,*,4))
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltFuncHostArrVec
  USE M
  IMPLICIT NONE
  TYPE(Child(4,20,20,4,20,4))  :: V(-20:13,4)

  CALL Sub(V(1:2, 2:3))
  ASSOCIATE  (W=>V(1, 2:2))
    IF ( ANY(W%Base%GetId() .NE. -1) ) STOP 54
    IF ( ANY(W%GetId()      .NE. -2) ) STOP 55
    IF ( ANY(W%BaseId       .NE. -1) ) STOP 56
    IF ( ANY(W%ChildId      .NE. -2) ) STOP 57
  END ASSOCIATE

  CONTAINS

  FUNCTION Fun(Arg)
  CLASS(*) :: Arg(:,:)
  CLASS(*), POINTER :: Fun(:,:)
    ALLOCATE(Fun(2:SIZE(Arg,1)+1,2:SIZE(Arg,2)+1), SOURCE=Arg)
  END FUNCTION

  SUBROUTINE Sub(Arg)
  CLASS(Child(4,*,*,4,*,4)), OPTIONAL :: Arg(1:2, 2)
  INTEGER :: S1(2)=(/1,1/)
  INTEGER :: S2(2)=(/1,1/)

    Arg(1,1)%BaseId = -1
    Arg(1,1)%ChildId = -2

    IF ( .NOT. PRESENT(Arg) ) STOP 11

    SELECT TYPE (U => Arg(S1,S2))
    CLASS IS (Child(4,*,*,4,*,4))
      SELECT TYPE (W => Fun(U))
      TYPE IS (Child(4,*,*,4,*,4))
          ! Check U
          IF ( SIZE(U)          .NE. 4 )          STOP 31
          IF ( ANY (LBOUND(U)   .NE. (/1, 1/) ) ) STOP 32
          IF ( ANY (UBOUND(U)   .NE. (/2, 2/) ) ) STOP 33
          IF ( ANY(SHAPE(U)     .NE. (/2,2/)) )   STOP 24
          IF ( ANY(U%Base%GetId() .NE. -1) )      STOP 35
          IF ( ANY(U%GetId()      .NE. -2) )      STOP 36
          IF ( ANY(U%BaseId       .NE. -1) )      STOP 37
          IF ( ANY(U%ChildId      .NE. -2) )      STOP 38

          !Check W
          IF ( SIZE(W)          .NE. 4 )          STOP 41
          IF ( ANY (LBOUND(W)   .NE. (/2, 2/) ) ) STOP 42
          IF ( ANY (UBOUND(W)   .NE. (/3, 3/) ) ) STOP 43
          IF ( ANY(SHAPE(W)     .NE. (/2,2/)) )   STOP 44
          IF ( ANY(W%Base%GetId() .NE. -1) )      STOP 45
          IF ( ANY(W%GetId()      .NE. -2) )      STOP 46
          IF ( ANY(W%BaseId       .NE. -1) )      STOP 47
          IF ( ANY(W%ChildId      .NE. -2) )      STOP 48

          IF ( .NOT. W%Called() ) STOP 45

       CLASS DEFAULT
          STOP 40
        CLASS is (Child(4,*,*,4,*,4))
          STOP 56
      END SELECT

  END SELECT

  END SUBROUTINE


  END



