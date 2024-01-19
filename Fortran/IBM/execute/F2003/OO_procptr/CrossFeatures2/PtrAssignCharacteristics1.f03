! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 18, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  proc-pointer-object is not elemental while proc-target may be elemental
!*  (315208)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM PtrAssignCharacteristics1
  IMPLICIT NONE

  ! The characteristics is the same as ABS
  !ABSTRACT INTERFACE
  INTERFACE
    FUNCTION F(A)
      REAL :: F
      REAL, INTENT(IN) :: A
    END FUNCTION
  END INTERFACE

  INTRINSIC ABS

  CALL IntSub(ABS)

  CONTAINS

  SUBROUTINE IntSub(Func)
  PROCEDURE(F)          :: Func
  PROCEDURE(F), POINTER :: ProcPtr

  IF (Func(-2.0) .NE. 2.0 ) ERROR STOP 11

  ProcPtr => ABS
  IF (ProcPtr(-2.0) .NE. 2.0 )  ERROR STOP 12
  IF (ProcPtr(2.0)  .NE. 2.0 )  ERROR STOP 12
  IF (ProcPtr(-0.0) .NE. 0.0 )  ERROR STOP 13

  ProcPtr => Func
  IF (ProcPtr(+2.0) .NE. 2.0 )                  ERROR STOP 22
  IF (ProcPtr(ProcPtr(-1.0)) .NE. 1.0 )         ERROR STOP 22
  IF (ProcPtr(-10000.0) .NE. Func(-10000.0) )   ERROR STOP 23

  END SUBROUTINE

  END

