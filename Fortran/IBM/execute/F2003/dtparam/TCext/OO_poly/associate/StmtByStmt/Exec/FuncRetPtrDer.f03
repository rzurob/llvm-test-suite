! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/FuncRetPtrDer.f
! *********************************************************************
!*  ===================================================================
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

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
      CLASS(Child(K1)), POINTER :: ChildArr(:) => NULL()
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

  PROGRAM FunRetPtrDer
  USE M
  IMPLICIT NONE
  TYPE(Child(4)), TARGET :: V = Child(4)(BaseId= -1, ChildId=-2 )

  ASSOCIATE ( As => Func( V ) )
    IF ( As%GetID()      .NE. -2) ERROR STOP 50
    IF ( As%Base%GetID() .NE. -1) ERROR STOP 51
    IF ( As%BaseId       .NE. -1) ERROR STOP 52
    IF ( As%ChildId      .NE. -2) ERROR STOP 53

    ASSOCIATE ( As1 => As%GetId() )
       IF ( As1 .NE. -2) ERROR STOP 54
    END ASSOCIATE

    IF ( .NOT. SAME_TYPE_AS(As, Child(4)()) )     ERROR STOP 55
    IF ( .NOT. ASSOCIATED(As%ChildArr) )       ERROR STOP 56
    IF ( .NOT. SAME_TYPE_AS(As%ChildArr, As) ) ERROR STOP 57

    IF ( ANY(LBOUND(As%ChildArr)  .NE. 1))     ERROR STOP 58
    IF ( ANY(UBOUND(As%ChildArr)  .NE. 3))     ERROR STOP 59
    IF ( As%ChildArr(1)%BaseId    .NE. 1)      ERROR STOP 60
    IF ( As%ChildArr(1)%ChildId   .NE. 2)      ERROR STOP 61
    IF ( ANY(As%ChildArr%Base%GetId() .NE. 1))      ERROR STOP 63
    IF ( ANY(As%ChildArr%GetId()      .NE. 2))      ERROR STOP 64
  END ASSOCIATE

  CONTAINS

  FUNCTION Func(Arg)
    CLASS(Child(4)), TARGET  :: Arg
    TYPE(Child(4)), POINTER  :: Func

    Func => Arg
    ALLOCATE( FUNC%ChildArr(3))

  END FUNCTION

  END
