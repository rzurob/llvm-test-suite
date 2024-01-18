! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/FuncRetAllocDer.f
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
!*
!*  TEST CASE NAME             : FuncRetAllocDer
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
!*    The selector is a func call returning an allocatable of derived type
!*    (Comp Failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
      CLASS(Base(K1)),  ALLOCATABLE :: BaseArr(:)
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4)), INTENT(IN)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM FunRetAllocDer
  USE M
  TYPE(Child(4)) :: V = Child(4)(BaseId= -1, ChildId=-2, BaseArr=NULL() )
  ASSOCIATE ( As => Func( V ) )
    IF ( As%GetID() .NE. -2) STOP 50
    IF ( As%BaseId  .NE. -1) STOP 51
    IF ( As%ChildId .NE. -2) STOP 52

    ASSOCIATE ( As1 => As%GetId() )
       IF ( As1 .NE. -2) STOP 52
    END ASSOCIATE

    IF ( .NOT. SAME_TYPE_AS(As, Child(4)( BaseArr=NULL())) )    STOP 53
    IF ( .NOT. SAME_TYPE_AS(As%BaseArr, As) ) STOP 54

    IF ( ANY(UBOUND(As%BaseArr) .NE. 3))     STOP 55
    IF ( As%BaseArr(1)%BaseId   .NE. 1)      STOP 56
    IF ( ANY(As%BaseArr%GetId() .NE. 2))     STOP 57
  END ASSOCIATE

  CONTAINS

  FUNCTION Func(Arg)
    CLASS(Child(4))              :: Arg
    TYPE(Child(4)), ALLOCATABLE  :: Func

    ALLOCATE(Func)
    Func = Arg
    ALLOCATE(Child(4) :: FUNC%BaseArr(3))

  END FUNCTION

  END
