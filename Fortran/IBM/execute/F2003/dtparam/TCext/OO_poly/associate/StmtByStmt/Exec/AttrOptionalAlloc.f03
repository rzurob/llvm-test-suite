! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/AttrOptionalAlloc.f
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
!*   The selector has an allocatable with
!*   the optional attribute
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
    CLASS(Base(4,*)), INTENT(INOUT) :: Arg
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base(4,*)), INTENT(INOUT)  :: Arg
      SELECT TYPE(Arg)
        TYPE IS (Child(4,*))
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE


  END MODULE


  PROGRAM AttrOptionalAlloc
  USE M
  INTEGER :: i
  TYPE (Child(4,:)), ALLOCATABLE :: W

  INTERFACE
    SUBROUTINE Sub(Arg, ArgT)
    IMPORT Child
    TYPE(Child(4,:)), OPTIONAL, ALLOCATABLE :: Arg
    TYPE(Child(4,:)), OPTIONAL, ALLOCATABLE :: ArgT
    END SUBROUTINE
  END INTERFACE

  CALL Sub(W, W)

  IF ( .NOT. ALLOCATED(W) )           ERROR STOP 50

  IF ( W%BaseID        .NE. 1 ) ERROR STOP 45
  IF ( W%Base%GetId()  .NE. 1 ) ERROR STOP 46
  IF ( W%ChildID       .NE. 2 ) ERROR STOP 47
  IF ( W%GetId()       .NE. 2 ) ERROR STOP 48

  END

  SUBROUTINE Sub(Arg, ArgT)
  USE M
  TYPE(Child(4,:)), OPTIONAL, ALLOCATABLE :: Arg
  TYPE(Child(4,:)), OPTIONAL, ALLOCATABLE :: ArgT

  IF ( .NOT. PRESENT(Arg) ) ERROR STOP  5

  ALLOCATE(Arg, SOURCE=Child(4,20)(BaseID=-1, ChildID=-2))

  IF ( .NOT. ALLOCATED(ArgT)) ERROR STOP 6
  ASSOCIATE ( Arg => Arg )

    IF ( Arg%BaseID        .NE. -1 ) ERROR STOP 30
    IF ( Arg%Base%GetId()  .NE. -1 ) ERROR STOP 31
    IF ( Arg%ChildID       .NE. -2 ) ERROR STOP 32
    IF ( Arg%GetId()       .NE. -2 ) ERROR STOP 33

    CALL Arg%Base%SetID(Arg)
    CALL Arg%SetID(Arg)

  END ASSOCIATE


  END SUBROUTINE
