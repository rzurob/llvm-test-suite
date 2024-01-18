! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltArrVec1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltArrVec1
!*
!*  DATE                       : Jan. 07, 2005
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
!*   The selector is a poly array constructor with vector subscript
!*    (wrong result: stop at 34)
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

    TYPE :: Test
      CLASS(*), POINTER :: ChildArr(:,:,:)
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
    CLASS(Base) :: Arg(:,:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base)  :: Arg(:,:,:)
      SELECT TYPE(Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltArrVec
  USE M
  IMPLICIT NONE
  INTEGER, PARAMETER :: Subs(18)=(/2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19/)
  TYPE(Child), TARGET :: Arr(2:20, 2:20, 2:20)
  TYPE(Test) :: V

  V%ChildArr => Arr

  SELECT TYPE ( V=>(/V%ChildArr(Subs, Subs, Subs)/) )
    CLASS DEFAULT
      SELECT TYPE (V => RESHAPE( V,(/18, 18, 18/) ))
      CLASS IS (Zero)
      SELECT TYPE (V)
        TYPE IS (Child)
          IF ( ANY (LBOUND(V)     .NE. (/1, 1, 1/) ) )    STOP 30
          IF ( ANY (UBOUND(V)     .NE. (/18, 18, 18/) ) ) STOP 31
          IF ( ANY (SHAPE(V)      .NE. (/18, 18, 18/) ) ) STOP 32

          IF ( ANY(V%Base%GetId() .NE. 1) ) STOP 34
          IF ( ANY(V%GetId()      .NE. 2) ) STOP 35
          IF ( ANY(V%BaseId       .NE. 1) ) STOP 36
          IF ( ANY(V%ChildId      .NE. 2) ) STOP 37

          CALL V(2,2,2)%SetId(V)
          CALL V(2,2,2)%Base%SetId(V%Base)

          IF ( ANY(V%Base%GetId() .NE. -1 ) ) STOP 44
          IF ( ANY(V%GetId()      .NE. -2 ) ) STOP 45
          IF ( ANY(V%BaseId       .NE. -1 ) ) STOP 46
          IF ( ANY(V%ChildId      .NE. -2 ) ) STOP 47

       CLASS DEFAULT
          STOP 40
        CLASS is (Child)
          STOP 56
        TYPE is (Base)
          STOP 57
        TYPE IS (Zero)
          STOP 58
      END SELECT
      END SELECT

  END SELECT

  END

