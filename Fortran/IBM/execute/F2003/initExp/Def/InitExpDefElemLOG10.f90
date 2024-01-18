!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 12, 2006
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
!*  a reference to an elemental intrinsic
!*
!*  -  LOG10
!*  (319120)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0
    REAL(4)  :: R4=10000.0
    REAL(8)  :: R8=100000000.0_8
    REAL(16) :: R6=10._16**16
  END TYPE

  TYPE(DT0), PARAMETER :: R(128)=DT0()

  CONTAINS

    FUNCTION ModFun(Arg1, Arg2, Arg3)
      REAL(16) :: ModFun
      REAL(4) :: Arg1
      REAL(8) :: Arg2
      REAL(16):: Arg3

      ModFun = Arg3 + Arg2 + Arg1
    END FUNCTION

  END MODULE


  PROGRAM  InitExpDefElemLOG10
  USE M
  IMPLICIT NONE
  INTEGER :: I, J


  INTERFACE
    FUNCTION F(Arg1, Arg2, Arg3)
      IMPORT
      REAL(INT(LOG10(R(128)%R6))) :: F
      REAL(INT(LOG10(R(128)%R4))) :: Arg1
      REAL(INT(LOG10(R(128)%R8))) :: Arg2
      REAL(INT(LOG10(R(128)%R6))) :: Arg3
    END FUNCTION
  END INTERFACE

  PROCEDURE(F), POINTER :: ProcPtr

  ProcPtr => ModFun

  IF ( ProcPtr(R(1)%R4, R(2)%R8, R(3)%R6) .NE. (R(1)%R4+R(2)%R8+R(3)%R6) )  STOP 11


  END


