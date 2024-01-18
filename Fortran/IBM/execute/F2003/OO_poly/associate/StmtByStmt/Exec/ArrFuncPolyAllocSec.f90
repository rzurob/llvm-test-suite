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
!*    The selector is a function call returning an allocatable array section
!*
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: Base
      SEQUENCE
      INTEGER :: BaseID=1
    END TYPE

    TYPE :: Child
      SEQUENCE
      TYPE(Base) :: BS
      INTEGER :: ChildID=2
    END TYPE

  CONTAINS

    FUNCTION ReturnArr(Arg)
    TYPE (Child) :: Arg(:)
    TYPE (Child), ALLOCATABLE :: ReturnArr(:)
      ALLOCATE(ReturnArr(SIZE(Arg)), SOURCE=Arg)
    END FUNCTION
  END MODULE

  PROGRAM ArrFuncPolyAllocSec
  USE M
  IMPLICIT NONE
  INTEGER :: i
  TYPE(Child) :: Arr(555)

  ASSOCIATE ( As => &
  &  ReturnArr((/ (Child(BS=Base(BaseID=-1), ChildID=-2), i=1,555)/) ) )

    IF ( ANY (LBOUND(As)  .NE. (/1/) ) )     STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/555/) ) )   STOP 31
    IF ( ANY (SHAPE(As)   .NE. (/555/) ) )   STOP 32
    IF ( ANY (As%ChildID    .NE. -2 ))         STOP 33
    IF ( ANY (As%BS%BaseID  .NE. -1 ))         STOP 34

    ASSOCIATE (A => As(1::2) )
      IF ( ANY (As%ChildID    .NE. -2 ))         STOP 33
      IF ( ANY (As%BS%BaseID  .NE. -1 ))         STOP 34
    END ASSOCIATE
  END ASSOCIATE

  END

