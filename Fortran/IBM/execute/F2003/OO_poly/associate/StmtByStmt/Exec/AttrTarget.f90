! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  AttrTarget.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : AttrTarget
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
!*   The selector has the target attribute
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


  PROGRAM AttrTarget
  USE M
  INTEGER :: i
  TYPE (Child),   TARGET :: W=Child(BaseID=-1, ChildID=-2)
  CLASS (Zero),  POINTER :: PtrZ
  CLASS (Base),  POINTER :: PtrB
  CLASS (Child), POINTER :: PtrC


  ASSOCIATE ( As => W )

    PtrZ => As%Base%Zero
    PtrB => As%Base
    PtrC => As

    IF ( .NOT. ASSOCIATED(PtrZ) ) STOP 20
    IF ( .NOT. ASSOCIATED(PtrB) ) STOP 21
    IF ( .NOT. ASSOCIATED(PtrC) ) STOP 22

    IF ( PtrB%BaseID      .NE. -1 ) STOP 30
    IF ( PtrB%GetId()     .NE. -1 ) STOP 31

    IF ( PtrC%BaseID        .NE. -1 ) STOP 30
    IF ( PtrC%Base%GetId()  .NE. -1 ) STOP 31
    IF ( PtrC%ChildID       .NE. -2 ) STOP 32
    IF ( PtrC%GetId()       .NE. -2 ) STOP 33

    ASSOCIATE ( As => W%Base )

      PtrZ => As%Zero
      PtrB => As

      IF ( .NOT. ASSOCIATED(PtrZ) ) STOP 20
      IF ( .NOT. ASSOCIATED(PtrB) ) STOP 21

      IF ( PtrB%BaseID      .NE. -1 ) STOP 30
      IF ( PtrB%GetId()     .NE. -1 ) STOP 31

    END ASSOCIATE

  END ASSOCIATE

  END
