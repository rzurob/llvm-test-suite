! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: ArrConstr1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ArrConstr1
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
!*
!*  (299176)
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

  PROGRAM ArrConstr1
  USE M
  IMPLICIT NONE
  TYPE(DT), TARGET :: V

  CALL Sub(V)

  CONTAINS

  SUBROUTINE Sub(U)
  CLASS(DT), TARGET :: U
  INTEGER :: i

  SELECT TYPE(U => (/(U%GetObj(U), i=1,4)/))
  CLASS IS (DT)
    IF (ANY(U%IArr(1) .NE. -1)) STOP 21
    IF (TRIM(U(1)%CArr(1)) .NE. "!") STOP 22
    IF (TRIM(U(4)%CArr(2)) .NE. "!") STOP 23
  CLASS DEFAULT
    STOP 40
  END SELECT

  SELECT TYPE (U => U%GetObj(U%DT1%DT0))
    CLASS IS (DT0)
      IF (ANY(U%IArr .NE. -1)) STOP 31
      IF (TRIM(U%CArr(1)) .NE. "!") STOP 32
      IF (TRIM(U%CArr(2)) .NE. "!") STOP 33
    CLASS DEFAULT
      STOP 50
  END SELECT

  END SUBROUTINE

  END



