!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Defect_328011_2.f
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
!*  Requested by 328011, covering passing array section into an elemental intrinsic
!*  function.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Defect_328011_2
  IMPLICIT NONE

  INTEGER            :: I
  INTEGER, PARAMETER :: ZZ(2,2) = RESHAPE((/1,2,3,4/),(/2,2/))
  INTEGER, PARAMETER :: YY(3)   = (/6,7,8/)
  INTEGER, PARAMETER :: AA(4)   = (/ (ZZ(1:i,1:i),i=1,1) , YY /)

  IF ( ANY( AA .NE. [1,6,7,8] ) ) STOP 11

  END


