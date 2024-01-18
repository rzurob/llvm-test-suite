! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  FuncArgPolyPtr.f  
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
!*  TEST CASE NAME             : FuncArgPolyPtr
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Nov. 02, 2004
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
!*    The poly associating entity is used as actual argument
!*    The associated entity is a pointer 
!*    (Comp failed: ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
      CLASS(Base),  POINTER :: BaseComp => NULL() 
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      CLASS(Child),  POINTER :: ChildComp => NULL() 
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId 
    END TYPE

    CONTAINS

    FUNCTION GetChildId(Arg)
    CLASS(Child) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    FUNCTION GetBaseId(Arg)
    CLASS(Base)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM FuncArgPolyPtr
  USE M
  CLASS(*),     ALLOCATABLE :: V1 
  CLASS(Base),  ALLOCATABLE :: V2 
  CLASS(Child), ALLOCATABLE :: V3 

  ALLOCATE(V1, SOURCE=GenChild(-1, -2))
  SELECT TYPE (V1)
    TYPE IS (Child)
      ALLOCATE(V2, SOURCE=V1)
    CLASS DEFAULT
      STOP 20
  END SELECT
  SELECT TYPE (V2)
    TYPE IS (Child)
      ALLOCATE(V3, SOURCE=V2)
    CLASS DEFAULT
      STOP 21
  END SELECT
 
  ASSOCIATE ( As => V1 )
  ASSOCIATE ( As1 => Func(As) )
    SELECT TYPE (As1)
      TYPE IS (Child)
        IF ( As1.GetId()      .NE. -2 ) STOP 30
        IF ( As1.Base%GetId() .NE. -1 ) STOP 31
      CLASS DEFAULT
        STOP 32
    END SELECT
  END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( As => V2 )
  ASSOCIATE ( As1 => Func(As) )
    SELECT TYPE (As1)
      TYPE IS (Child)
        IF ( As1.GetId()      .NE. -2 ) STOP 40
        IF ( As1.Base%GetId() .NE. -1 ) STOP 41
      CLASS DEFAULT
        STOP 42
    END SELECT
  END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( As => V3 )
  ASSOCIATE ( As1 => Func(As) )
    SELECT TYPE (As1)
      TYPE IS (Child)
        IF ( As1.GetId()      .NE. -2 ) STOP 50
        IF ( As1.Base%GetId() .NE. -1 ) STOP 51
      CLASS DEFAULT
        STOP 52
    END SELECT
  END ASSOCIATE
  END ASSOCIATE

  CONTAINS

  FUNCTION GenChild(Arg1, Arg2)
  CLASS(*), ALLOCATABLE :: GenChild
  INTEGER               :: Arg1, Arg2
    ALLOCATE(GenChild, SOURCE=Child(BaseId=Arg1, ChildId=Arg2))
  END FUNCTION
  
  FUNCTION Func(Arg)
    CLASS(*), TARGET  :: Arg
    CLASS(*), POINTER  :: Func

    Func => Arg 
 
  END FUNCTION 

  END
