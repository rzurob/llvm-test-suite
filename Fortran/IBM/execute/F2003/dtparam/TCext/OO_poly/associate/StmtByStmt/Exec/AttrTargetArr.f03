! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/AttrTargetArr.f
! *********************************************************************
!*  ===================================================================
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


  PROGRAM AttrTargetArr
  USE M
  TYPE (Child(4,20)),   TARGET :: W(3:5)=Child(4,20)(BaseID=-1, ChildID=-2)
  CLASS (Zero(4,:)),  POINTER :: PtrZ(:)
  CLASS (Base(4,:)),  POINTER :: PtrB(:)
  CLASS (Child(4,:)), POINTER :: PtrC(:)


  ASSOCIATE ( As => W )

    PtrZ => As%Base%Zero
    PtrB => As%Base
    PtrC => As

    IF ( .NOT. ASSOCIATED(PtrZ) ) ERROR STOP 20
    IF ( .NOT. ASSOCIATED(PtrB) ) ERROR STOP 21
    IF ( .NOT. ASSOCIATED(PtrC) ) ERROR STOP 22

    IF( ANY(LBOUND(PtrZ) .NE. (/1/) ) ) ERROR STOP 30
    IF( ANY(LBOUND(PtrB) .NE. (/1/) ) ) ERROR STOP 31
    IF( ANY(LBOUND(PtrC) .NE. (/3/) ) ) ERROR STOP 32

    IF( ANY(UBOUND(PtrZ) .NE. (/3/) ) ) ERROR STOP 40
    IF( ANY(UBOUND(PtrB) .NE. (/3/) ) ) ERROR STOP 41
    IF( ANY(UBOUND(PtrC) .NE. (/5/) ) ) ERROR STOP 42

    IF( ANY(SHAPE(PtrZ)  .NE. (/3/) ) ) ERROR STOP 50
    IF( ANY(SHAPE(PtrB)  .NE. (/3/) ) ) ERROR STOP 51
    IF( ANY(SHAPE(PtrC)  .NE. (/3/) ) ) ERROR STOP 52

    IF ( ANY(PtrB%BaseID      .NE. -1 )) ERROR STOP 60
    IF ( ANY(PtrB%GetId()     .NE. -1 )) ERROR STOP 61

    IF ( ANY(PtrC%BaseID        .NE. -1 )) ERROR STOP 62
    IF ( ANY(PtrC%Base%GetId()  .NE. -1 )) ERROR STOP 63
    IF ( ANY(PtrC%ChildID       .NE. -2 )) ERROR STOP 64
    IF ( ANY(PtrC%GetId()       .NE. -2 )) ERROR STOP 65

    ASSOCIATE ( As => As%Base )

      PtrZ => As%Zero
      PtrB => As

      IF ( .NOT. ASSOCIATED(PtrZ) ) ERROR STOP 70
      IF ( .NOT. ASSOCIATED(PtrB) ) ERROR STOP 71

      IF( ANY(LBOUND(PtrZ) .NE. (/1/) ) ) ERROR STOP 80
      IF( ANY(LBOUND(PtrB) .NE. (/1/) ) ) ERROR STOP 81

      IF( ANY(UBOUND(PtrZ) .NE. (/3/) ) ) ERROR STOP 90
      IF( ANY(UBOUND(PtrB) .NE. (/3/) ) ) ERROR STOP 91

      IF( ANY(SHAPE(PtrZ)  .NE. (/3/) ) ) ERROR STOP 93
      IF( ANY(SHAPE(PtrB)  .NE. (/3/) ) ) ERROR STOP 94

      IF ( ANY(PtrB%BaseID      .NE. -1 )) ERROR STOP 95
      IF ( ANY(PtrB%GetId()     .NE. -1 )) ERROR STOP 96

    END ASSOCIATE

  END ASSOCIATE

  END