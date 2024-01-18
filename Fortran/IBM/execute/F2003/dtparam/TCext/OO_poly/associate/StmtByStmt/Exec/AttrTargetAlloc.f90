! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/AttrTargetAlloc.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  AttrTargetAlloc.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : AttrTargetAlloc
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
!*   The selector has the target and allocatable attributes
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
      private
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
    CLASS(Base(4,*)) :: Arg(:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base(4,*))  :: Arg(:)
      SELECT TYPE(Arg)
        TYPE IS (Child(4,*))
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE


  END MODULE


  PROGRAM AttrTargetAlloc
  USE M
  INTEGER :: i
  TYPE (Child(4,:)),   TARGET, ALLOCATABLE :: W
  CLASS (Zero(4,:)),  POINTER :: PtrZ
  CLASS (Base(4,:)),  POINTER :: PtrB
  CLASS (Child(4,:)), POINTER :: PtrC

  ALLOCATE(W, SOURCE=Child(4,20)(BaseID=-1, ChildID=-2))

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
