!*********************************************************************
!*  ===================================================================
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
!*  -  RADIX
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM   InitExpDefInqRADIX
  IMPLICIT NONE
  INTEGER :: I, J, K


  REAL(4),   PARAMETER :: R4 = 10
  REAL(8),   PARAMETER :: R8(-2147483648:-2147483647, 2147483646:2147483647) = 1
  REAL(16),  PARAMETER :: R6(1:0) = -1

  INTEGER  :: TR4(16, 16)  = RESHAPE((/(RADIX(R4), I=1,256)/), (/16,16/))
  INTEGER  :: TR8(16, 16)  = RESHAPE((/(RADIX(R8), I=1,256)/), (/16,16/))
  INTEGER  :: TR6(16, 16)  = RESHAPE((/(RADIX(R6), I=1,256)/), (/16,16/))

  INTEGER(1),  PARAMETER :: I1(1:0) = 0
  INTEGER(2),  PARAMETER :: I2(0:0) = -1
  INTEGER(4),  PARAMETER :: I4 = 10
  INTEGER(8),  PARAMETER :: I8(-2147483648:-2147483647, 2147483646:2147483647) = 0

  INTEGER  :: TI1(16, 16)  = RESHAPE((/(RADIX(I1), I=1,256)/), (/16,16/))
  INTEGER  :: TI2(16, 16)  = RESHAPE((/(RADIX(I2), I=1,256)/), (/16,16/))
  INTEGER  :: TI4(16, 16)  = RESHAPE((/(RADIX(I4), I=1,256)/), (/16,16/))
  INTEGER  :: TI8(16, 16)  = RESHAPE((/(RADIX(I8), I=1,256)/), (/16,16/))


  IF ( ANY ( TR4  .NE. 2  )  )   STOP 11
  IF ( ANY ( TR8  .NE. 2  )  )   STOP 12
  IF ( ANY ( TR6  .NE. 2  )  )   STOP 13

  IF ( ANY ( TI1  .NE. 2  )  )   STOP 21
  IF ( ANY ( TI2  .NE. 2  )  )   STOP 22
  IF ( ANY ( TI4  .NE. 2  )  )   STOP 23
  IF ( ANY ( TI8  .NE. 2  )  )   STOP 24

  END



