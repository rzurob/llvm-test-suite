! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodeferredlp -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltFuncHostArrPolyPtrSec.f
! opt variations: -qnok -qnol -qdeferredlp -qreuse=none

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
!*   a array section pointer
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

    TYPE, EXTENDS(Zero)  :: Base    ! (4,20)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1)  :: ChildId = 2
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
    CLASS(Child(4,*)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4,*)), INTENT(INOUT) :: Arg(:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base(4,*)), INTENT(INOUT)  :: Arg(:,:)
      SELECT TYPE(Arg)
        TYPE IS (Child(4,*))
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltFuncHostArrPolyPtrSec
  USE M
  IMPLICIT NONE
  CLASS(Child(4,20)), POINTER :: V(:,:)

  CALL Sub(V)
  SELECT TYPE (W=>V)
  CLASS IS (Child(4,*))
    IF ( ANY(W%Base%GetId() .NE. -1) ) ERROR STOP 54
    IF ( ANY(W%GetId()      .NE. -2) ) ERROR STOP 55
    IF ( ANY(W%BaseId       .NE. -1) ) ERROR STOP 56
    IF ( ANY(W%ChildId      .NE. -2) ) ERROR STOP 57
  END SELECT

  CONTAINS

  FUNCTION Fun(Arg)
  CLASS(*) :: Arg(:,:)
  CLASS(*), POINTER :: Fun(:,:)
    ALLOCATE(Fun(2:SIZE(Arg,1)+1,2:SIZE(Arg,2)+1), SOURCE=Arg)
  END FUNCTION

  SUBROUTINE Sub(Arg)
  CLASS(Child(4,20)),OPTIONAL, POINTER :: Arg(:, :)

    IF ( .NOT. PRESENT(Arg) ) ERROR STOP 11

    ALLOCATE(Arg(2:3,3:4))
    Arg%BaseId = -1
    Arg%ChildId = -2

    SELECT TYPE (U => Arg(:,::1))
    CLASS IS (Child(4,*))
      SELECT TYPE (W => Fun(U(::1,:)))
      TYPE IS (Child(4,*))
          ! Check U
          IF ( SIZE(U)          .NE. 4 )          ERROR STOP 31
          IF ( ANY (LBOUND(U)   .NE. (/1, 1/) ) ) ERROR STOP 32
          IF ( ANY (UBOUND(U)   .NE. (/2, 2/) ) ) ERROR STOP 33
          IF ( ANY(SHAPE(U)     .NE. (/2,2/)) )   ERROR STOP 24
          IF ( ANY(U%Base%GetId() .NE. -1) )      ERROR STOP 35
          IF ( ANY(U%GetId()      .NE. -2) )      ERROR STOP 36
          IF ( ANY(U%BaseId       .NE. -1) )      ERROR STOP 37
          IF ( ANY(U%ChildId      .NE. -2) )      ERROR STOP 38

          !Check W
          IF ( SIZE(W)          .NE. 4 )          ERROR STOP 41
          IF ( ANY (LBOUND(W)   .NE. (/2, 2/) ) ) ERROR STOP 42
          IF ( ANY (UBOUND(W)   .NE. (/3, 3/) ) ) ERROR STOP 43
          IF ( ANY(SHAPE(W)     .NE. (/2,2/)) )   ERROR STOP 44
          IF ( ANY(W%Base%GetId() .NE. -1) )      ERROR STOP 45
          IF ( ANY(W%GetId()      .NE. -2) )      ERROR STOP 46
          IF ( ANY(W%BaseId       .NE. -1) )      ERROR STOP 47
          IF ( ANY(W%ChildId      .NE. -2) )      ERROR STOP 48

          IF ( .NOT. W%Called() ) ERROR STOP 45

       CLASS DEFAULT
          STOP 40
        CLASS is (Child(4,*))
          STOP 56
      END SELECT

  END SELECT

  END SUBROUTINE


  END



