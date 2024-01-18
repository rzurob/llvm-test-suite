! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SelectCase.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SelectCase
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
!* (ICE-299302)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM SelectCase
  IMPLICIT CLASS(*)(U)
  TYPE :: DT
    INTEGER :: Int
    CHARACTER(3) :: C
  END TYPE
  INTEGER :: i

  CALL Sub(DT(Int=6, C=""), 6)

  CONTAINS

  SUBROUTINE Sub(U, I)

  SELECT TYPE (U)
  CLASS IS (DT)

    SELECT CASE (U%Int)
    CASE (:5)
      STOP 20
    CASE (6)
      PRINT *, "1-OK!"
    CASE (7:)
      STOP 20
    END SELECT


    SELECT CASE (I)
    CASE (:5)
      STOP 20
    CASE (6)
      PRINT *, "2-OK!"
    CASE (7:)
      STOP 20
    END SELECT


  CLASS DEFAULT
    STOP 40
  END SELECT

  END SUBROUTINE

  END



