!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 25, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qrealsize
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  -qstrict
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpSTRICT
  IMPLICIT NONE

  INTEGER  :: I
  REAL     :: R
  COMPLEX  :: Z

  REAL(4),       PARAMETER :: R4A(128)=(/( 1., I=0, 127)/)
  REAL(4),       PARAMETER :: R4B(128)=(/(-0., I=0, 127)/)
  REAL(4),       PARAMETER :: R4Arr(128) =SIGN(A=R4A, B=R4B)

  REAL(8),       PARAMETER :: R8A(128)=(/( 1., I=0, 127)/)
  REAL(8),       PARAMETER :: R8B(128)=(/(-0., I=0, 127)/)
  REAL(8),       PARAMETER :: R8Arr(128) =SIGN(A=R4A, B=R4B)

  REAL(16),      PARAMETER :: R6A(128)=(/( 1., I=0, 127)/)
  REAL(16),      PARAMETER :: R6B(128)=(/(-0., I=0, 127)/)
  REAL(16),      PARAMETER :: R6Arr(128) =SIGN(A=R4A, B=R4B)


  IF ( ANY( R4Arr  .NE. -1 ))      STOP 11

  IF ( ANY( R8Arr  .NE. -1 ))      STOP 12

  IF ( ANY( R6Arr  .NE. -1 ))      STOP 13



  END


