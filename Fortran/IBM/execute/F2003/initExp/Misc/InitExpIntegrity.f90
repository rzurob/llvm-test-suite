!*********************************************************************
!*  ===================================================================
!*
!*  TESTOP CASE NAME             : InitExpIntegrity.f
!*  TESTOP CASE TITLE            :
!*
!*  DATE                       : Semp. 06 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER IntegrityTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Integrity of parentheses
!*
!*  (324919)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpIntegrity

  REAL,    PARAMETER :: A(128)=1
  INTEGER, PARAMETER :: B(128)=1
  INTEGER, PARAMETER :: C(128)=3

  INTEGER(KIND=1)  :: I1(128)= A*(B/C)
  INTEGER(KIND=2)  :: I2(128)= A*(A*(B/C))
  INTEGER(KIND=4)  :: I4(128)= A*(B/C)*A
  INTEGER(KIND=8)  :: I8(128)= ((B/C)*A)

  REAL(4),  PARAMETER :: F(128)=HUGE(1._4)
  REAL(8),  PARAMETER :: G(128)=HUGE(1._8)
  REAL(16), PARAMETER :: H(128)=HUGE(1._16)
  REAL(16), PARAMETER :: T(128)=TINY(1._16)

  ! otherwise, NaN
  REAL(4) :: R4(128)=F+(F-F)
  REAL(8) :: R8(128)=(G-G) + G
! REAL(16):: R6(128)=H+(H + (-H) )   ! There is an IBM limitation on REAL(16) --  HUGE(0._16) - 0._16
  REAL(16):: R6(128)=T + (H + (-H) )



  IF ( ANY(I1   .NE. 0 ) ) STOP 11
  IF ( ANY(I2   .NE. 0 ) ) STOP 12
  IF ( ANY(I4   .NE. 0 ) ) STOP 13
  IF ( ANY(I8   .NE. 0 ) ) STOP 14


  IF ( ANY(R4   .NE. F ) ) STOP 31
  IF ( ANY(R8   .NE. G ) ) STOP 32
! IF ( ANY(R6   .NE. H ) ) STOP 33
  IF ( ANY(R6   .NE. T ) ) STOP 33


  END



