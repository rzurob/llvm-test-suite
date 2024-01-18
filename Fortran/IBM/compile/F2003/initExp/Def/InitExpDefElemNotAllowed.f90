!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 17, 2006
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
!*  a reference to an elemental intrinsic function that is not a hardware-specific, service/utility,
!*  floating-point status/control, or vector intrinsic function, where each argument is
!*  an initialization expression;
!*  - vector is not qualified.
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpDefElemNotAllowed
  IMPLICIT NONE

  VECTOR(INTEGER(4)) ::  A,B
  INTEGER(4) :: R1=VEC_ALL_GE(B,A)

  VECTOR(INTEGER(1)) :: R2=VEC_ABS(A)

  END


