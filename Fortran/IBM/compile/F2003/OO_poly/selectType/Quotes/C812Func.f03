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
!*    The selector is a function call, associate name appears in variablr
!*    definition context
!*
!*    (Wrong Msg: "The selector in the SELECT TYPE statement is not a named variable.
!*     An associate name should appear" )
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM C812Func
  IMPLICIT NONE

  TYPE :: Base
  END TYPE

  TYPE, EXTENDS(Base) :: Child
    TYPE(Base) :: BaseArr(1)
  END TYPE

  SELECT TYPE ( As => Fun(Base()) )
    TYPE IS (Base)
      STOP 20
    CLASS DEFAULT
      STOP 30
    CLASS IS (Child)
      As = Child( BaseArr=(/Base()/) )
  END SELECT
  STOP 40

  CONTAINS

  FUNCTION Fun(Arg)
  TYPE(Base) :: Arg
  CLASS(Base), POINTER :: Fun
    ALLOCATE( Fun, SOURCE=Child(BaseArr=(/Base()/) ) )
    SELECT TYPE( Fun )
      TYPE IS (Child)
        Fun%Base=Arg
      CLASS DEFAULT
        STOP 22
    END SELECT
  END FUNCTION

  END

