!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 31, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : DTPARAM: Automatic objects
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 333321
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  "DT" has only parent component
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM autoobj85

  TYPE Base(n)
    INTEGER, LEN :: n
    INTEGER :: a(n)
  END TYPE

  TYPE, EXTENDS(Base) :: DT
  END TYPE

  TYPE(DT(4)) :: x
  x%a = [1,2,3,4]

  CALL sub(4)
  CONTAINS

  SUBROUTINE Sub(N)

  INTEGER :: N
  TYPE(DT(N)) :: Y

  Y = X  ! <- segfault because tpv unset& prolog missing!

  IF ( Y%N .NE. 4 ) stop 11
  IF ( ANY(Y%BASE%A .NE. [1,2,3,4]) ) stop 12

  END SUBROUTINE
  END
