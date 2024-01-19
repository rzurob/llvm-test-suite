! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 04, 2005
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
!* "1516-078 (S) Operands must be conformable" and incorrect output
!*  for "print*, As%BaseArr(1,1)%BaseId"
!* (297811 )
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE Zero
    END TYPE

    TYPE, EXTENDS(Zero) :: Base
      INTEGER :: BaseId = 1
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      TYPE(Base) :: BaseArr(1,1)
    END TYPE

  END MODULE

  PROGRAM Misc17
  USE M
  IMPLICIT NONE
  CLASS(Base), ALLOCATABLE :: Var
  integer i

  ALLOCATE(Child :: Var)

  SELECT TYPE ( As  => RESHAPE( (/(Var,  i=1,4)/), (/2,2/)) )
  TYPE IS (Child)

    print*, SHAPE(As)
    print*, SHAPE(As%BaseArr(1,1)%BaseId)
    print*, As%BaseArr(1,1)%BaseId
    IF ( ANY (SHAPE(As%BaseArr(1,1)%BaseId) .NE. (/2,2/) ) )  ERROR STOP 35
    IF ( ANY(As%BaseArr(1,1)%BaseId  .NE. 1) ) ERROR STOP 36

  END SELECT

  END

