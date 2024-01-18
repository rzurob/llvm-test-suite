! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  AttrTargetArr.f  
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : AttrTargetArr 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb 22, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 219934
!*
!*  DRIVER STANZA              :
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


  PROGRAM AttrTargetArr 
  USE M
  TYPE (Child),   TARGET :: W(3:5)=Child(BaseID=-1, ChildID=-2)
  CLASS (Zero),  POINTER :: PtrZ(:)
  CLASS (Base),  POINTER :: PtrB(:)
  CLASS (Child), POINTER :: PtrC(:)


  ASSOCIATE ( As => W )

    PtrZ => As%Base%Zero 
    PtrB => As%Base 
    PtrC => As 
 
    IF ( .NOT. ASSOCIATED(PtrZ) ) STOP 20
    IF ( .NOT. ASSOCIATED(PtrB) ) STOP 21
    IF ( .NOT. ASSOCIATED(PtrC) ) STOP 22

    IF( ANY(LBOUND(PtrZ) .NE. (/1/) ) ) STOP 30
    IF( ANY(LBOUND(PtrB) .NE. (/1/) ) ) STOP 31
    IF( ANY(LBOUND(PtrC) .NE. (/3/) ) ) STOP 32
 
    IF( ANY(UBOUND(PtrZ) .NE. (/3/) ) ) STOP 40
    IF( ANY(UBOUND(PtrB) .NE. (/3/) ) ) STOP 41
    IF( ANY(UBOUND(PtrC) .NE. (/5/) ) ) STOP 42
 
    IF( ANY(SHAPE(PtrZ)  .NE. (/3/) ) ) STOP 50
    IF( ANY(SHAPE(PtrB)  .NE. (/3/) ) ) STOP 51
    IF( ANY(SHAPE(PtrC)  .NE. (/3/) ) ) STOP 52
 
    IF ( ANY(PtrB%BaseID      .NE. -1 )) STOP 60
    IF ( ANY(PtrB%GetId()     .NE. -1 )) STOP 61

    IF ( ANY(PtrC%BaseID        .NE. -1 )) STOP 62
    IF ( ANY(PtrC%Base%GetId()  .NE. -1 )) STOP 63
    IF ( ANY(PtrC%ChildID       .NE. -2 )) STOP 64
    IF ( ANY(PtrC%GetId()       .NE. -2 )) STOP 65

    ASSOCIATE ( As => As%Base )
     
      PtrZ => As%Zero 
      PtrB => As 
 
      IF ( .NOT. ASSOCIATED(PtrZ) ) STOP 70
      IF ( .NOT. ASSOCIATED(PtrB) ) STOP 71

      IF( ANY(LBOUND(PtrZ) .NE. (/1/) ) ) STOP 80
      IF( ANY(LBOUND(PtrB) .NE. (/1/) ) ) STOP 81
 
      IF( ANY(UBOUND(PtrZ) .NE. (/3/) ) ) STOP 90
      IF( ANY(UBOUND(PtrB) .NE. (/3/) ) ) STOP 91
 
      IF( ANY(SHAPE(PtrZ)  .NE. (/3/) ) ) STOP 93
      IF( ANY(SHAPE(PtrB)  .NE. (/3/) ) ) STOP 94
 
      IF ( ANY(PtrB%BaseID      .NE. -1 )) STOP 95
      IF ( ANY(PtrB%GetId()     .NE. -1 )) STOP 96

    END ASSOCIATE

  END ASSOCIATE

  END
