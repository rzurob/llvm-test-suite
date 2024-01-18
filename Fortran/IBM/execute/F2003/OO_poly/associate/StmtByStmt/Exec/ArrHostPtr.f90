! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  ArrHostPtr.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ArrHostPtr
!*
!*  DATE                       : Feb 14, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is a host associate name
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
      private
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
    CLASS(Base) :: Arg(:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base)  :: Arg(:)
      SELECT TYPE(Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM ArrHostPtr
  USE M
  IMPLICIT NONE
  TYPE(Child), TARGET :: Tar

  CLASS(Child), POINTER :: Arr(:)

  ALLOCATE(Arr(3), SOURCE=Child(ChildId=-2, BaseId=-1))

  CALL Sub(Arr)

  CONTAINS

  SUBROUTINE Sub(Arr1)
  CLASS(Child), POINTER :: Arr1(:)

  ASSOCIATE ( As => Arr1(:))
    SELECT TYPE ( As => As)
    CLASS IS (Child)
      ASSOCIATE (As => As(:), As1 => Arr(::1))
          IF (ANY(SHAPE(As) .NE. (/3/)))      STOP 33
          IF ( ANY(As%Base%GetId() .NE. -1) ) STOP 34
          IF ( ANY(As%GetId()      .NE. -2) ) STOP 35
          IF ( ANY(As%BaseId       .NE. -1) ) STOP 36
          IF ( ANY(As%ChildId      .NE. -2) ) STOP 37

          CALL As1(1)%SetId(As1)
          CALL As1(1)%Base%SetId(As1%Base)

          IF ( ANY(As%Base%GetId() .NE. 1 ) ) STOP 44
          IF ( ANY(As%GetId()      .NE. 2 ) ) STOP 45
          IF ( ANY(As%BaseId       .NE. 1 ) ) STOP 46
          IF ( ANY(As%ChildId      .NE. 2 ) ) STOP 47

      END ASSOCIATE

    CLASS DEFAULT
      STOP 38

  END SELECT

  END ASSOCIATE

  END SUBROUTINE

  END



