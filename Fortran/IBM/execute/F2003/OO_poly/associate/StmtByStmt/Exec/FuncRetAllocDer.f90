! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  FuncRetAllocDer.f  
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
!*  TEST CASE NAME             : FuncRetAllocDer 
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
!*    The selector is a func call returning an allocatable of derived type 
!*    (Comp Failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      CLASS(Base),  ALLOCATABLE :: BaseArr(:)
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId 
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM FunRetAllocDer 
  USE M
  TYPE(Child) :: V = Child(BaseId= -1, ChildId=-2, BaseArr=NULL() )
  ASSOCIATE ( As => Func( V ) )
    IF ( As%GetID() .NE. -2) STOP 50 
    IF ( As%BaseId  .NE. -1) STOP 51 
    IF ( As%ChildId .NE. -2) STOP 52 

    ASSOCIATE ( As1 => As%GetId() )
       IF ( As1 .NE. -2) STOP 52 
    END ASSOCIATE
   
    IF ( .NOT. SAME_TYPE_AS(As, Child( BaseArr=NULL())) )    STOP 53
    IF ( .NOT. SAME_TYPE_AS(As%BaseArr, As) ) STOP 54

    IF ( ANY(UBOUND(As%BaseArr) .NE. 3))     STOP 55
    IF ( As%BaseArr(1)%BaseId   .NE. 1)      STOP 56
    IF ( ANY(As%BaseArr%GetId() .NE. 2))     STOP 57
  END ASSOCIATE

  CONTAINS

  FUNCTION Func(Arg)
    CLASS(Child)              :: Arg
    TYPE(Child), ALLOCATABLE  :: Func

    ALLOCATE(Func)
    Func = Arg 
    ALLOCATE(Child :: FUNC%BaseArr(3))
 
  END FUNCTION 

  END
