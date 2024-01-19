! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=none /tstdev/OO_poly/selectType/Quotes/C812DefOp.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self -qreuse=base

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 3, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C812
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
!*    The selector is an exp with defined operator
!*    the associate name appears in var definition context
!*
!*    (Wrong Msg)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: Base(K1)    ! (4)
        INTEGER, KIND :: K1
    END TYPE

    TYPE, EXTENDS(Base) :: Child(K2)    ! (4,4)
      INTEGER, KIND  :: K2
      INTEGER(K2)    :: ChildId = 2
      TYPE(Base(K2)) :: BaseArr(1)= Base(K2)()
    END TYPE

  END MODULE

  PROGRAM C812DefOp
  USE M
  IMPLICIT NONE

  INTERFACE OPERATOR ( .OP. )
    FUNCTION MyOp (Arg1, Arg2)
      IMPORT Base, Child
      TYPE(Base(4)),  INTENT(IN) :: Arg1
      TYPE(Child(4,4)), INTENT(IN) :: Arg2
      CLASS(*), ALLOCATABLE   :: MyOp
    END FUNCTION
  END INTERFACE OPERATOR ( .OP. )

  SELECT TYPE ( As => Base(4)() .OP. Child(4,4)(ChildId=-2) )
    TYPE IS (Base(4))
      STOP 20
    CLASS DEFAULT
      STOP 30
    CLASS IS (Child(4,4))
      As = Child(4,4)()
  END SELECT
  STOP 40

  END


  FUNCTION MyOp (Arg1, Arg2)
  USE M
  TYPE(Base(4)),  INTENT(IN)   :: Arg1
  TYPE(Child(4,4)), INTENT(IN)   :: Arg2
  CLASS(*),    ALLOCATABLE  :: MyOp
    ALLOCATE( MyOp, SOURCE=Arg2 )
  END FUNCTION


