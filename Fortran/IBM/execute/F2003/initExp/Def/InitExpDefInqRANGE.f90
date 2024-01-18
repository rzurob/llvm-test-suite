!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefInqRANGE.f
!*
!*  DATE                       : Apr. 04, 2006
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
!*  a reference to a specification inquiry
!*
!*  -  RANGE
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDefInqRANGE
  IMPLICIT NONE
  INTEGER :: I, J, K


  INTEGER(1),  PARAMETER :: I1(1:0) = 0
  INTEGER(2),  PARAMETER :: I2(0:0) = -1
  INTEGER(4),  PARAMETER :: I4 = 10
  INTEGER(8),  PARAMETER :: I8(-2147483648:-2147483647, 2147483646:2147483647) = 0

  INTEGER  :: TI1(16, 16)  = RESHAPE((/(RANGE(I1), I=1,256)/), (/16,16/))
  INTEGER  :: TI2(16, 16)  = RESHAPE((/(RANGE(I2), I=1,256)/), (/16,16/))
  INTEGER  :: TI4(16, 16)  = RESHAPE((/(RANGE(I4), I=1,256)/), (/16,16/))
  INTEGER  :: TI8(16, 16)  = RESHAPE((/(RANGE(I8), I=1,256)/), (/16,16/))

  REAL(4),   PARAMETER :: R4 = 10
  REAL(8),   PARAMETER :: R8(-2147483648:-2147483647, 2147483646:2147483647) = 1
  REAL(16),  PARAMETER :: R6(1:0) = -1

  INTEGER  :: TR4(1, 2) = RESHAPE((/RANGE(R4), INT( MIN(LOG10(HUGE(R4)),-LOG10(TINY(R4))))/), (/1,2/))
  INTEGER  :: TR8(1, 2) = RESHAPE((/RANGE(R8), INT( MIN(LOG10(HUGE(R8)),-LOG10(TINY(R8))))/), (/1,2/))
  INTEGER  :: TR6(1, 2) = RESHAPE((/RANGE(R6), INT( MIN(LOG10(HUGE(R6)),-LOG10(TINY(R6))))/), (/1,2/))

  COMPLEX(4),   PARAMETER :: Z4 = 10
  COMPLEX(8),   PARAMETER :: Z8(-2147483648:-2147483647, 2147483646:2147483647) = 1
  COMPLEX(16),  PARAMETER :: Z6(1:0) = -1

  INTEGER  :: TZ4(1, 2) = RESHAPE((/RANGE(Z4), RANGE(REAL(Z4))/), (/1,2/))
  INTEGER  :: TZ8(1, 2) = RESHAPE((/RANGE(Z8), RANGE(REAL(Z8))/), (/1,2/))
  INTEGER  :: TZ6(1, 2) = RESHAPE((/RANGE(Z6), RANGE(REAL(Z6))/), (/1,2/))


  IF ( ANY ( TI1  .NE. INT(LOG10(REAL(HUGE(I1))))  )  )   STOP 11
  IF ( ANY ( TI2  .NE. INT(LOG10(REAL(HUGE(I2))))  )  )   STOP 12
  IF ( ANY ( TI4  .NE. INT(LOG10(REAL(HUGE(I4))))  )  )   STOP 14
  IF ( ANY ( TI8  .NE. INT(LOG10(REAL(HUGE(I8))))  )  )   STOP 18

  IF ( ANY ( TR4(:, 1)  .NE. TR4(:, 2) ) )   STOP 21
  IF ( ANY ( TR8(:, 1)  .NE. TR8(:, 2) ) )   STOP 22
  IF ( ANY ( TR6(:, 1)  .NE. TR6(:, 2) ) )   STOP 23

  IF ( ANY ( TZ4(:, 1)  .NE. TZ4(:, 2) ) )   STOP 31
  IF ( ANY ( TZ8(:, 1)  .NE. TZ8(:, 2) ) )   STOP 32
  IF ( ANY ( TZ6(:, 1)  .NE. TZ6(:, 2) ) )   STOP 33

  END



