! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  FuncRetPtrDer.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : FuncRetPtrDer
!*
!*  DATE                       : Nov. 02, 2004
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
!*    The selector is a func call returning a pointer of derived type
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
      CLASS(Child), POINTER :: ChildArr(:) => NULL()
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
    CLASS(Base), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM FunRetPtrDer
  USE M
  IMPLICIT NONE
  TYPE(Child), TARGET :: V = Child(BaseId= -1, ChildId=-2 )

  ASSOCIATE ( As => Func( V ) )
    IF ( As%GetID()      .NE. -2) STOP 50
    IF ( As%Base%GetID() .NE. -1) STOP 51
    IF ( As%BaseId       .NE. -1) STOP 52
    IF ( As%ChildId      .NE. -2) STOP 53

    ASSOCIATE ( As1 => As%GetId() )
       IF ( As1 .NE. -2) STOP 54
    END ASSOCIATE

    IF ( .NOT. SAME_TYPE_AS(As, Child()) )     STOP 55
    IF ( .NOT. ASSOCIATED(As%ChildArr) )       STOP 56
    IF ( .NOT. SAME_TYPE_AS(As%ChildArr, As) ) STOP 57

    IF ( ANY(LBOUND(As%ChildArr)  .NE. 1))     STOP 58
    IF ( ANY(UBOUND(As%ChildArr)  .NE. 3))     STOP 59
    IF ( As%ChildArr(1)%BaseId    .NE. 1)      STOP 60
    IF ( As%ChildArr(1)%ChildId   .NE. 2)      STOP 61
    IF ( ANY(As%ChildArr%Base%GetId() .NE. 1))      STOP 63
    IF ( ANY(As%ChildArr%GetId()      .NE. 2))      STOP 64
  END ASSOCIATE

  CONTAINS

  FUNCTION Func(Arg)
    CLASS(Child), TARGET  :: Arg
    TYPE(Child), POINTER  :: Func

    Func => Arg
    ALLOCATE( FUNC%ChildArr(3))

  END FUNCTION

  END
