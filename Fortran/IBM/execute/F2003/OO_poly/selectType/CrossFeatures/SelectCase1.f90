! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SelectCase1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SelectCase1
!*
!*  DATE                       : Feb. 04, 2005
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
!* Select Case
!* (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM SelectCase1
  IMPLICIT CLASS(*)(U)
  TYPE :: DT
    INTEGER :: Int
    CHARACTER(3) :: C
  END TYPE
  INTEGER :: i

  CALL Sub(6_8)

  CONTAINS

  SUBROUTINE Sub(U)

A:SELECT TYPE (U)
  TYPE IS (INTEGER(4))
    STOP 30
  TYPE IS (INTEGER(8))

B:  SELECT CASE (U)
    CASE (:5_8)
      STOP 20
    CASE (6_8)
      PRINT *, "OK!"
    CASE (7_8:)
      STOP 20
    END SELECT B

  CLASS DEFAULT
    STOP 40
  END SELECT A

  END SUBROUTINE

  END



