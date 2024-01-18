! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 28, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
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
!*
!*  The selector is an array constructor
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
    TYPE  :: DT0
      INTEGER(4)      :: IArr(2)=-1
      CHARACTER(1025) :: CArr(2)="!"
    END TYPE

    TYPE, ABSTRACT, EXTENDS(DT0) :: DT1
      CLASS(DT0), POINTER, PRIVATE :: Ptr
      CONTAINS
      PROCEDURE, NoPASS   :: GetObj
    END TYPE

    TYPE, EXTENDS(DT1) :: DT
      PRIVATE
    END TYPE

    CONTAINS

    FUNCTION GetObj(Arg)
    CLASS(*),TARGET, INTENT(IN) :: Arg
    CLASS(*), POINTER  :: GetObj
      GetObj => Arg
    END FUNCTION

  END MODULE

  PROGRAM ArrConstr3
  USE M
  IMPLICIT NONE
  TYPE(DT), TARGET :: V(2,2)

  CALL Sub(V)

  CONTAINS

  SUBROUTINE Sub(U)
  CLASS(DT), TARGET :: U(:,:)
  INTEGER :: i

  SELECT TYPE(U => (/U/))
  CLASS IS (DT)
  SELECT TYPE(U => (/U,U/))
  CLASS IS (DT)

    IF (ANY(U%IArr(1) .NE. -1)) STOP 21
    IF (TRIM(U(1)%CArr(1)) .NE. "!") STOP 22
    IF (TRIM(U(8)%CArr(2)) .NE. "!") STOP 23

  CLASS DEFAULT
    STOP 40
  END SELECT
  CLASS DEFAULT
    STOP 41
  END SELECT

  SELECT TYPE (U => (/(U%GetObj(U(1,1)%DT1%DT0), i=1,4) /) )
    CLASS IS (DT0)
      ASSOCIATE(U => (/U,U(4)/))
        IF (ANY(U%IArr(1) .NE. -1)) STOP 31
        IF (ANY(U%IArr(2) .NE. -1)) STOP 31
        IF (TRIM(U(1)%CArr(1)) .NE. "!") STOP 32
        IF (TRIM(U(5)%CArr(2)) .NE. "!") STOP 33
      END ASSOCIATE
    CLASS DEFAULT
      STOP 50
  END SELECT

  END SUBROUTINE

  END



