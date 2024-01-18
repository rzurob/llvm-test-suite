! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: InClassIsClassDefault2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InClassIsClassDefault2
!*
!*  DATE                       : Jan. 25, 2005
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
!*  Within the CLASS DEFAULT and TYPE IS
!*  for extensible types
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  MODULE M

    TYPE  :: Zero
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
    CLASS(Base) :: Arg(:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base)  :: Arg(:,:)
      SELECT TYPE(Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM InClassIsClassDefault2
  USE M
  IMPLICIT NONE

  TYPE(Child) :: Arr(2:3,3:4)

  CALL Sub(Arr)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg(2:3,3:4)

    SELECT TYPE (U => Arg)
    CLASS DEFAULT
    SELECT TYPE (U)
    CLASS IS (Child)
    SELECT TYPE (U)
    CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, Arg))       STOP 30
      IF ( SIZE(U)          .NE. 4 )          STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/2, 3/) ) ) STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/3, 4/) ) ) STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/2,2/)) )   STOP 34

      SELECT TYPE ( As => Arg )

      TYPE IS (Child)

          IF ( ANY(As%Base%GetId() .NE. U%Base%GetId()) ) STOP 34
          IF ( ANY(As%GetId()      .NE. U%GetId()) )      STOP 35
          IF ( ANY(As%BaseId       .NE. U%BaseId) )       STOP 36
          IF ( ANY(As%ChildId      .NE. U%ChildId) )      STOP 37

          CALL As(2,3)%SetId(As)
          CALL As(2,3)%Base%SetId(As%Base)

          IF ( ANY(As%Base%GetId() .NE. U%Base%GetId()) ) STOP 44
          IF ( ANY(As%GetId()      .NE. U%GetId()) )      STOP 45
          IF ( ANY(As%BaseId       .NE. U%BaseId) )       STOP 46
          IF ( ANY(As%ChildId      .NE. U%ChildId) )      STOP 47

      CLASS DEFAULT
        STOP 51
      END SELECT

    END SELECT
    END SELECT
    END SELECT

  END SUBROUTINE

  END



