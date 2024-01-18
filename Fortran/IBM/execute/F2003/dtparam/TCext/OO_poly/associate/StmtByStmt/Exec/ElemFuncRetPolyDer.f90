! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ElemFuncRetPolyDer.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  ElemFuncRetPolyDer.f  
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
!*  TEST CASE NAME             : ElemFuncRetPolyDer 
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
!*    The selector is an elemental func call returning a poly var 
!*    of derived type 
!*    ( Comp Failed)
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
    CLASS(Base(4)), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM ElemFuncRetPolyDer  
  USE M
  Type(Child(4)) :: V(2, 2) 
  
  ASSOCIATE ( As => Func( V ) )
    IF ( .NOT. ALL ( SHAPE(As) .EQ. (/2,2/)) ) STOP 81 
    IF( ANY(As%BaseId       .NE. -1) ) STOP 46
    IF( ANY(As%Base%GetId() .NE. -1) ) STOP 47
    IF( ANY(As%GetId()      .NE. -2) ) STOP 48
    IF( ANY(As%ChildId      .NE. -2) ) STOP 49
  END ASSOCIATE

  CONTAINS

  ELEMENTAL FUNCTION Func(Arg)
    CLASS(Base(4)), INTENT(IN)  :: Arg
    TYPE(Child(4))              :: Func

    ASSOCIATE ( As => Arg)
      SELECT TYPE (As )
      TYPE IS (Child(4))
        Func = Child(4)(BaseId=-1, ChildId=-2) 
      END SELECT
    END ASSOCIATE
  END FUNCTION 

  END
