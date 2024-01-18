! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltFuncHostArrPolyPtrSec.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltFuncHostArrPolyPtrSec
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

    TYPE  :: Zero
    CONTAINS
      PROCEDURE, NoPASS   :: Called
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
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
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base), INTENT(INOUT) :: Arg(:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base), INTENT(INOUT)  :: Arg(:,:)
      SELECT TYPE(Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltFuncHostArrPolyPtrSec
  USE M
  IMPLICIT NONE
  CLASS(Child), POINTER :: V(:,:)

  CALL Sub(V)
  SELECT TYPE (W=>V)
  CLASS IS (Child)
    IF ( ANY(W%Base%GetId() .NE. -1) ) STOP 54
    IF ( ANY(W%GetId()      .NE. -2) ) STOP 55
    IF ( ANY(W%BaseId       .NE. -1) ) STOP 56
    IF ( ANY(W%ChildId      .NE. -2) ) STOP 57
  END SELECT

  CONTAINS

  FUNCTION Fun(Arg)
  CLASS(*) :: Arg(:,:)
  CLASS(*), POINTER :: Fun(:,:)
    ALLOCATE(Fun(2:SIZE(Arg,1)+1,2:SIZE(Arg,2)+1), SOURCE=Arg)
  END FUNCTION

  SUBROUTINE Sub(Arg)
  CLASS(Child),OPTIONAL, POINTER :: Arg(:, :)

    IF ( .NOT. PRESENT(Arg) ) STOP 11

    ALLOCATE(Arg(2:3,3:4))
    Arg%BaseId = -1
    Arg%ChildId = -2

    SELECT TYPE (U => Arg(:,::1))
    CLASS IS (Child)
      SELECT TYPE (W => Fun(U(::1,:)))
      TYPE IS (Child)
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
        CLASS is (Child)
          STOP 56
      END SELECT

  END SELECT

  END SUBROUTINE


  END



