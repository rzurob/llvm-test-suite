!*********************************************************************
!*  ===================================================================
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
!*  Unary :  +/-
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpIntrinOpUnary
  IMPLICIT NONE

  INTEGER :: I

  INTEGER(1), PARAMETER :: I11(128)=+(/(-I, I=1, 128)/)
  INTEGER(2), PARAMETER :: I21(128)=+(/(-I, I=1, 128)/)
  INTEGER(4), PARAMETER :: I41(128)=+(/(-I, I=1, 128)/)
  INTEGER(8), PARAMETER :: I81(128)=+(/(-I, I=1, 128)/)

  INTEGER(1), PARAMETER :: I12(128)=-(/(+I, I=127, 0, -1)/)
  INTEGER(2), PARAMETER :: I22(128)=-(/(+I, I=127, 0, -1)/)
  INTEGER(4), PARAMETER :: I42(128)=-(/(+I, I=127, 0, -1)/)
  INTEGER(8), PARAMETER :: I82(128)=-(/(+I, I=127, 0, -1)/)

  REAL(4),  PARAMETER :: R41(128)=+(/(-I, I=1, 128)/)
  REAL(8),  PARAMETER :: R81(128)=+(/(-I, I=1, 128)/)
  REAL(16), PARAMETER :: R61(128)=+(/(-I, I=1, 128)/)

  REAL(4),  PARAMETER :: R42(128)=-(/(+I, I=127, 0, -1)/)
  REAL(8),  PARAMETER :: R82(128)=-(/(+I, I=127, 0, -1)/)
  REAL(16), PARAMETER :: R62(128)=-(/(+I, I=127, 0, -1)/)

  COMPLEX(4),  PARAMETER :: Z41(128)=+(/(-(+I,0.), I=1, 128)/)
  COMPLEX(8),  PARAMETER :: Z81(128)=+(/(-(+I,0.), I=1, 128)/)
  COMPLEX(16), PARAMETER :: Z61(128)=+(/(-(+I,0.), I=1, 128)/)

  COMPLEX(4),  PARAMETER :: Z42(128)=-(/(+(+I,0.), I=127, 0, -1)/)
  COMPLEX(8),  PARAMETER :: Z82(128)=-(/(+(+I,0.), I=127, 0, -1)/)
  COMPLEX(16), PARAMETER :: Z62(128)=-(/(+(+I,0.), I=127, 0, -1)/)

  INTEGER(1) :: IT1(128) = I41 + I12
  INTEGER(2) :: IT2(128) = I21 + I82
  INTEGER(4) :: IT4(128) = I11 + I22
  INTEGER(8) :: IT8(128) = I11 + I12

  REAL(4)   :: RT4(128) = I11 + R62
  REAL(8)   :: RT8(128) = R41 + R42
  REAL(16)  :: RT6(128) = I21 + R82

  COMPLEX(4)   :: ZT4(128) = R41 + I82
  COMPLEX(8)   :: ZT8(128) = Z41 + Z62
  COMPLEX(16)  :: ZT6(128) = I11 + R42

  IF ( ANY(IT1  .NE. -128 ) ) STOP 11
  IF ( ANY(IT2  .NE. -128 ) ) STOP 12
  IF ( ANY(IT4  .NE. -128 ) ) STOP 13
  IF ( ANY(IT8  .NE. -128 ) ) STOP 14

  IF ( ANY(RT4  .NE. -128 ) ) STOP 21
  IF ( ANY(RT8  .NE. -128 ) ) STOP 22
  IF ( ANY(RT6  .NE. -128 ) ) STOP 23

  IF ( ANY(ZT4  .NE. (-128,0) ) ) STOP 31
  IF ( ANY(ZT8  .NE. -128     ) ) STOP 32
  IF ( ANY(ZT6  .NE. -128     ) ) STOP 33


  END


