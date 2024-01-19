!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 17, 2006
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
!*  Requested by 328011, covering:
!*  1 --  passing array section into an elemental intrinsic function.
!*  2 --  passing array section whose bounds change into an elemental intrinsic function.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Defect_328011_1
  IMPLICIT NONE
  INTEGER :: I, J, K
  REAL(8), PARAMETER    :: r(8)=1.5574077_8
  REAL(8) :: T(8)=(/(ATAN(r(I:I)),  I=1,8)/)
  REAL(8) :: S(5)=(/(ATAN(r(1:I+1)),  I=1,2)/)

  IF ( ANY( ABS(T - 1.0) .GE. 1.E-8 ) ) ERROR STOP 11
  IF ( ANY( ABS(S - 1.0) .GE. 1.E-8 ) ) ERROR STOP 12

  END


