!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpTypSpecProcPtr2.f
!*
!*  DATE                       : Aug. 29, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  intrinsic-type-spec in proc ptr: char
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS

  FUNCTION C1()
    CHARACTER(1) :: C1
    C1 = "1"
  END FUNCTION

  FUNCTION C2()
    CHARACTER(2) :: C2
    C2 = "12"
  END FUNCTION

  FUNCTION C3()
    CHARACTER(3) :: C3
    C3 = "123"
  END FUNCTION



  END MODULE


  PROGRAM InitExpTypSpecProcPtr2
  USE M
  IMPLICIT NONE

  INTEGER :: I
  CHARACTER, POINTER :: C(:)

  PROCEDURE(CHARACTER(KIND=C%KIND)),                POINTER :: C1ProcPtr
  PROCEDURE(CHARACTER(LEN=C%LEN+1,  KIND=C%KIND)),  POINTER :: C2ProcPtr
  PROCEDURE(CHARACTER(C%LEN+2,  KIND=C%KIND)),      POINTER :: C3ProcPtr


  C1ProcPtr => C1
  C2ProcPtr => C2
  C3ProcPtr => C3


  IF ( C1ProcPtr() .NE. "1"    ) STOP 11
  IF ( C2ProcPtr() .NE. "12"   ) STOP 12
  IF ( C3ProcPtr() .NE. "123"  ) STOP 13

  END


