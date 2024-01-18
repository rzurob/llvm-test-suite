! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  FuncRetPolyAllocArr.f  
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
!*  TEST CASE NAME             : FuncRetPolyAllocArr 
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
!*    The selector is a func call returning a poly allocatable 
!*    array of derived type 
!*    (Comp Failed : 298097)
!*    (ICE-298465 on same_type_as())
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
      CLASS(Child), POINTER  :: ChildComp => NULL() 
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
    CLASS(Base), INTENT (IN)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM FunRetPolyAllocArr
  USE M
  TYPE(Child), TARGET :: V = Child(BaseId= -1, ChildId=-2 )

  ASSOCIATE ( As => Func( V ) )
    ASSOCIATE( As1 => As(3:3) )
      IF( ANY( As1%GetId()      .NE. (/2/)) )  STOP 46
      IF( As1(1)%ChildComp%GetId() .NE. -2)    STOP 47
    END ASSOCIATE

    IF ( .NOT. ASSOCIATED(As(3)%BaseComp, V) )  STOP 48
    IF ( .NOT. ASSOCIATED(As(3)%ChildComp, V) ) STOP 49

    IF ( ANY(As%GetID() .NE. 2))  STOP 50 
    IF ( ANY(As%BaseId  .NE. 1) ) STOP 51 

    ASSOCIATE ( As1 => As(1)%ChildComp%GetId() )
       IF ( As1 .NE. -2) STOP 52 
    END ASSOCIATE
   
    IF ( .NOT. SAME_TYPE_AS(As, Child()) )         STOP 53
    IF ( .NOT. SAME_TYPE_AS(As(3)%BaseComp, As) )  STOP 54
    IF ( .NOT. SAME_TYPE_AS(As(1)%ChildComp, As) ) STOP 55

    IF ( As(3)%BaseComp%BaseId   .NE. -1 )  STOP 56
    IF ( As(2)%ChildComp%ChildId .NE. -2 )  STOP 57

  END ASSOCIATE

  CONTAINS

  FUNCTION Func(Arg)
    CLASS(Child), TARGET     :: Arg
    CLASS(Child), ALLOCATABLE :: Func(:)

    ALLOCATE(Child :: Func(3)) 
    FUNC(3)%BaseComp  => Arg
    SELECT TYPE (As => Func )
      TYPE IS (Child) 
        AS(1)%ChildComp => Arg 
        AS(2)%ChildComp => Arg 
        AS(3)%ChildComp => Arg 
      CLASS DEFAULT
        STOP 77
    END SELECT

  END FUNCTION 

  END
