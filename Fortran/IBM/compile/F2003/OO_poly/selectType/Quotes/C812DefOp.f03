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
    TYPE :: Base
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER    :: ChildId = 2
      TYPE(Base) :: BaseArr(1)= Base()
    END TYPE

  END MODULE

  PROGRAM C812DefOp
  USE M
  IMPLICIT NONE

  INTERFACE OPERATOR ( .OP. )
    FUNCTION MyOp (Arg1, Arg2)
      IMPORT Base, Child
      TYPE(Base),  INTENT(IN) :: Arg1
      TYPE(Child), INTENT(IN) :: Arg2
      CLASS(*), ALLOCATABLE   :: MyOp
    END FUNCTION
  END INTERFACE OPERATOR ( .OP. )

  SELECT TYPE ( As => Base() .OP. Child(ChildId=-2) )
    TYPE IS (Base)
      STOP 20
    CLASS DEFAULT
      STOP 30
    CLASS IS (Child)
      As = Child()
  END SELECT
  STOP 40

  END


  FUNCTION MyOp (Arg1, Arg2)
  USE M
  TYPE(Base),  INTENT(IN)   :: Arg1
  TYPE(Child), INTENT(IN)   :: Arg2
  CLASS(*),    ALLOCATABLE  :: MyOp
    ALLOCATE( MyOp, SOURCE=Arg2 )
  END FUNCTION


