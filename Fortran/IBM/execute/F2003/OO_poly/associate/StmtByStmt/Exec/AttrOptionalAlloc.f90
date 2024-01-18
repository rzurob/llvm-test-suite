! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  AttrOptionalAlloc.f  
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
!*  TEST CASE NAME             : AttrOptionalAlloc
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
!*   The selector has an allocatable with 
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
    CLASS(Base), INTENT(INOUT) :: Arg
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base), INTENT(INOUT)  :: Arg
      SELECT TYPE(Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE


  END MODULE


  PROGRAM AttrOptionalAlloc
  USE M
  INTEGER :: i
  TYPE (Child), ALLOCATABLE :: W
  
  INTERFACE
    SUBROUTINE Sub(Arg, ArgT)
    IMPORT Child 
    TYPE(Child), OPTIONAL, ALLOCATABLE :: Arg
    TYPE(Child), OPTIONAL, ALLOCATABLE :: ArgT
    END SUBROUTINE 
  END INTERFACE

  CALL Sub(W, W)

  IF ( .NOT. ALLOCATED(W) )           STOP 50

  IF ( W%BaseID        .NE. 1 ) STOP 45
  IF ( W%Base%GetId()  .NE. 1 ) STOP 46
  IF ( W%ChildID       .NE. 2 ) STOP 47
  IF ( W%GetId()       .NE. 2 ) STOP 48

  END 

  SUBROUTINE Sub(Arg, ArgT)
  USE M
  TYPE(Child), OPTIONAL, ALLOCATABLE :: Arg
  TYPE(Child), OPTIONAL, ALLOCATABLE :: ArgT

  IF ( .NOT. PRESENT(Arg) ) STOP  5

  ALLOCATE(Arg, SOURCE=Child(BaseID=-1, ChildID=-2))

  IF ( .NOT. ALLOCATED(ArgT)) STOP 6
  ASSOCIATE ( Arg => Arg )

    IF ( Arg%BaseID        .NE. -1 ) STOP 30
    IF ( Arg%Base%GetId()  .NE. -1 ) STOP 31
    IF ( Arg%ChildID       .NE. -2 ) STOP 32
    IF ( Arg%GetId()       .NE. -2 ) STOP 33
  
    CALL Arg%Base%SetID(Arg)
    CALL Arg%SetID(Arg)

  END ASSOCIATE


  END SUBROUTINE

