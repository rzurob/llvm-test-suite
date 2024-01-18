! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  AttrOptionalArrAlloc.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : AttrOptionalArrAlloc
!*
!*  DATE                       : Feb 22, 2005
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
!*
!*   The selector has an allocatable arrray with
!*   the optional attribute
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
    CLASS(Base), INTENT(INOUT) :: Arg(:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base), INTENT(INOUT)  :: Arg(:)
      SELECT TYPE(Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE


  END MODULE


  PROGRAM AttrOptionalArrAlloc
  USE M
  INTEGER :: i
  TYPE (Child), ALLOCATABLE :: W(:)

  INTERFACE
    SUBROUTINE Sub(Arg, I, J)
    IMPORT Child
    TYPE(Child), OPTIONAL, ALLOCATABLE :: Arg(:)
    INTEGER               :: I, J
    END SUBROUTINE
  END INTERFACE

  CALL Sub(W, 3, 8)

  IF ( .NOT. ALLOCATED(W) )           STOP 50
  IF ( ANY(LBOUND(W) .NE. (/3/) ) )   STOP 52
  IF ( ANY(SHAPE(W)  .NE. (/6/) ) )   STOP 53

  IF ( ANY(W(::2)%BaseID        .NE.  1 )) STOP 40
  IF ( ANY(W(::2)%Base%GetId()  .NE.  1 )) STOP 41
  IF ( ANY(W(::2)%ChildID       .NE.  2 )) STOP 42
  IF ( ANY(W(::2)%GetId()       .NE.  2 )) STOP 43

  IF ( ANY(W(4::2)%BaseID        .NE. 0 )) STOP 45
  IF ( ANY(W(4::2)%Base%GetId()  .NE. 0 )) STOP 46
  IF ( ANY(W(4::2)%ChildID       .NE. 0 )) STOP 47
  IF ( ANY(W(4::2)%GetId()       .NE. 0 )) STOP 48

  END

  SUBROUTINE Sub(Arg, I, J)
  USE M
  TYPE(Child), OPTIONAL, ALLOCATABLE :: Arg(:)
  INTEGER :: I, J

  IF ( .NOT. PRESENT(Arg) ) STOP  05

  ALLOCATE(Arg(I:J))
  Arg(::2) = Child(BaseID=-1, ChildID=-2)
  Arg(4::2)= Child(BaseID=-0, ChildID=-0)

  ASSOCIATE ( Arg => Arg(::2) )

    IF ( ANY(LBOUND(Arg) .NE. (/1/) ) )         STOP 12
    IF ( ANY(SHAPE(Arg)  .NE. (/(J-I+1)/2/) ) ) STOP 13

    IF ( ANY(Arg%BaseID        .NE. -1 )) STOP 30
    IF ( ANY(Arg%Base%GetId()  .NE. -1 )) STOP 31
    IF ( ANY(Arg%ChildID       .NE. -2 )) STOP 32
    IF ( ANY(Arg%GetId()       .NE. -2 )) STOP 33

    CALL Arg%Base%SetID(Arg)
    CALL Arg%SetID(Arg)

  END ASSOCIATE


  END SUBROUTINE

