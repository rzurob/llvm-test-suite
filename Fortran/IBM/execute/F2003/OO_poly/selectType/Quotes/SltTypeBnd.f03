! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 14, 2004
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
!*   The type spec is specified with a type with variuos bindings
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER, PRIVATE :: BaseId=-1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, ABSTRACT,  EXTENDS(Base) :: Base1
      INTEGER :: Base1Id=1
    CONTAINS
      PROCEDURE(GetBase1Interface), PASS, DEFERRED, PRIVATE   :: GetIdx
      PROCEDURE, PASS  :: GetBase1Id
    END TYPE

    TYPE, EXTENDS(Base1) :: Child
    ! PRIVATE
      INTEGER :: ChildId=2
      TYPE(Base)  :: Base1Comp=Base(BaseId=0)
    CONTAINS
      PROCEDURE, PASS  :: GetId => GetChildId
      PROCEDURE, PASS  :: GetIdx => GetChildId ! bypass the original private attr ?
    END TYPE

    INTERFACE
      ELEMENTAL FUNCTION GetBase1Interface(Arg)
        IMPORT Base1
        CLASS(Base1), INTENT(IN) :: Arg
        INTEGER      :: GetBase1Interface
      END FUNCTION
    END INTERFACE

    CONTAINS

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    FUNCTION GetBase1Id(Arg)
    CLASS(Base1)  :: Arg
    INTEGER      :: GetBase1Id
      GetBase1Id = Arg%Base1Id
    END FUNCTION

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

  END MODULE

  PROGRAM SltTypeBnd
  USE M
  IMPLICIT NONE

  CLASS(*) ,ALLOCATABLE :: Var

  ALLOCATE(Var, SOURCE=Child())

  SELECT TYPE (Var)
   CLASS IS (Base)
     STOP 20
   TYPE IS (Base)
     STOP 21
   CLASS IS (Child)
     STOP 22
   TYPE IS (Child)
     IF (Var%GetIdx()           .NE. 2 )    ERROR STOP 31
     IF (Var%GetId()            .NE. 2 )    ERROR STOP 32
     IF (Var%ChildId            .NE. 2 )    ERROR STOP 33
     IF (Var%Base1Id            .NE. 1 )    ERROR STOP 34
     IF (Var%GetBase1Id()       .NE. 1 )    ERROR STOP 35
     IF (Var%Base%GetId()       .NE. -1 )   ERROR STOP 36
     IF (Var%Base1%Base%GetId() .NE. -1 )   ERROR STOP 37
     IF (Var%Base1%Base%GetId() .NE. -1 )   ERROR STOP 38
!    IF (Var%Base1%BaseId       .NE. -1 )   ERROR STOP 30 ! Private
     IF (Var%Base1%Base1Id      .NE.  1 )   ERROR STOP 39

!    IF (Var%BaseId  .NE. -1 )      ERROR STOP 31  ! Private
!    IF (Var%Base1%GetBase1Id() .NE. 1 ) ERROR STOP 32 ! this is wrong
  END SELECT

  END
