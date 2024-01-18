! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/FuncRetPolyAllocArr.f
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

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND            :: K1
      INTEGER(K1)              :: BaseId = 1
      CLASS(Base(K1)), POINTER :: BaseComp => NULL() 
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
      CLASS(Child(K1)), POINTER  :: ChildComp => NULL() 
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
    CLASS(Base(4)), INTENT (IN)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM FunRetPolyAllocArr
  USE M
  TYPE(Child(4)), TARGET :: V = Child(4)(BaseId= -1, ChildId=-2 )

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
   
    IF ( .NOT. SAME_TYPE_AS(As, Child(4)()) )         STOP 53
    IF ( .NOT. SAME_TYPE_AS(As(3)%BaseComp, As) )  STOP 54
    IF ( .NOT. SAME_TYPE_AS(As(1)%ChildComp, As) ) STOP 55

    IF ( As(3)%BaseComp%BaseId   .NE. -1 )  STOP 56
    IF ( As(2)%ChildComp%ChildId .NE. -2 )  STOP 57

  END ASSOCIATE

  CONTAINS

  FUNCTION Func(Arg)
    CLASS(Child(4)), TARGET     :: Arg
    CLASS(Child(4)), ALLOCATABLE :: Func(:)

    ALLOCATE(Child(4) :: Func(3)) 
    FUNC(3)%BaseComp  => Arg
    SELECT TYPE (As => Func )
      TYPE IS (Child(4)) 
        AS(1)%ChildComp => Arg 
        AS(2)%ChildComp => Arg 
        AS(3)%ChildComp => Arg 
      CLASS DEFAULT
        STOP 77
    END SELECT

  END FUNCTION 

  END
