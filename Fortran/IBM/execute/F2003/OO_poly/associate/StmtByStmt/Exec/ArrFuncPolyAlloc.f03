! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 14, 2005
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
!*    The selector is a function call returning an allocatable array of poly
!*    (Comp failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: Base
      INTEGER :: BaseID=1
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER :: ChildID=2
    END TYPE

  CONTAINS

    FUNCTION ReturnArr(Arg)
    CLASS (Base) :: Arg(:)
    CLASS (Base), ALLOCATABLE :: ReturnArr(:)
      ALLOCATE(ReturnArr(SIZE(Arg)), SOURCE=Arg)
    END FUNCTION
  END MODULE

  PROGRAM ArrFuncPolyAlloc
  USE M
  IMPLICIT NONE
  INTEGER :: i
  TYPE(Child) :: Arr(555)=Child(BaseID=-1, ChildID=-2)

  ASSOCIATE ( As => ReturnArr(Arr(:)) )

    IF ( ANY (LBOUND(As)  .NE. (/1/) ) )     ERROR STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/555/) ) )   ERROR STOP 31
    IF ( ANY (SHAPE(As)   .NE. (/555/) ) )   ERROR STOP 32
    IF ( ANY (As%BaseID   .NE. -1 ))         ERROR STOP 33

    SELECT TYPE ( AS => ReturnArr(As))
    TYPE IS (Child)
      IF ( ANY (LBOUND(As)  .NE. (/1/) ) )     ERROR STOP 40
      IF ( ANY (UBOUND(As)  .NE. (/555/) ) )   ERROR STOP 41
      IF ( ANY (SHAPE(As)   .NE. (/555/) ) )   ERROR STOP 42
      IF ( ANY (As%BaseID  .NE. -1 ))          ERROR STOP 44
    CLASS DEFAULT
      STOP 35
    END SELECT

    IF ( ANY (Arr%ChildID  .NE. -2 ))       ERROR STOP 54

  END ASSOCIATE

  END

