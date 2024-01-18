!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          BM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : nitExpIntrinOpRelation.f  
!*  TEST CASE TTLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Aug. 29, 2006
!*  ORGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289074 
!*
!*  DRVER STANZA              :
!*  REQURED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDTIONS :
!*
!*  DESCRPTION
!*
!*  
!*  .EQ., .NEQV. 
!*  .GT., .GE., .LT., .LE. 
!*  
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM nitExpIntrinOpRelation 
  IMPLICIT NONE

  INTEGER :: I

  INTEGER(1), PARAMETER :: I11(128)=(/(-I, I=1, 128)/)
  INTEGER(2), PARAMETER :: I21(128)=(/(-I, I=1, 128)/)
  INTEGER(4), PARAMETER :: I41(128)=(/(-I, I=1, 128)/)
  INTEGER(8), PARAMETER :: I81(128)=(/(-I, I=1, 128)/)

  INTEGER(1), PARAMETER :: I12(128)=(/(I, I=0, 127)/)
  INTEGER(2), PARAMETER :: I22(128)=(/(I, I=0, 127)/)
  INTEGER(4), PARAMETER :: I42(128)=(/(I, I=0, 127)/)
  INTEGER(8), PARAMETER :: I82(128)=(/(I, I=0, 127)/)

  REAL(4),    PARAMETER :: R41(128)=(/(-I, I=1, 128)/)
  REAL(8),    PARAMETER :: R81(128)=(/(-I, I=1, 128)/)
  REAL(16),   PARAMETER :: R61(128)=(/(-I, I=1, 128)/)

  REAL(4),     PARAMETER :: R42(128)=(/(I, I=0, 127)/)
  REAL(8),     PARAMETER :: R82(128)=(/(I, I=0, 127)/)
  REAL(16),    PARAMETER :: R62(128)=(/(I, I=0, 127)/)

  COMPLEX(4),  PARAMETER :: Z41(128)=(/(-I, I=1, 128)/)
  COMPLEX(8),  PARAMETER :: Z81(128)=(/(-I, I=1, 128)/)
  COMPLEX(16), PARAMETER :: Z61(128)=(/(-I, I=1, 128)/)

  COMPLEX(4),  PARAMETER :: Z42(128)=(/(I, I=0, 127)/)
  COMPLEX(8),  PARAMETER :: Z82(128)=(/(I, I=0, 127)/)
  COMPLEX(16), PARAMETER :: Z62(128)=(/(I, I=0, 127)/)

  LOGICAL(1) :: LI1(128) = I41 .GT. I12
  LOGICAL(2) :: LI2(128) = I21 .GE. R82
  LOGICAL(4) :: LI4(128) = I11 .LT. I21
  LOGICAL(8) :: LI8(128) = I11 .LE. I12

  LOGICAL(4) :: LR4(128) = I11 ==   R62
  LOGICAL(8) :: LR8(128) = R41 /=   R42
  LOGICAL(2) :: LR6(128) = I21 .LE. R82

  LOGICAL(4) :: LZ4(128) = R41 .EQ. I82
  LOGICAL(8) :: LZ8(128) = Z41 /=   Z62
  LOGICAL(1) :: LZ6(128) = I11 .NE. R42

  IF ( ANY(LI1  .NEQV. .FALSE. ) ) STOP 11
  IF ( ANY(LI2  .NEQV. .FALSE. ) ) STOP 12
  IF ( ANY(LI4  .NEQV. .FALSE. ) ) STOP 13
  IF ( ANY(LI8  .NEQV. .TRUE.  ) ) STOP 14

  IF ( ANY(LR4  .NEQV. .FALSE. ) ) STOP 21
  IF ( ANY(LR8  .NEQV. .TRUE.  ) ) STOP 22
  IF ( ANY(LR6  .NEQV. .TRUE.  ) ) STOP 23

  IF ( ANY(LZ4  .NEQV. .FALSE. ) ) STOP 31
  IF ( ANY(LZ8  .NEQV. .TRUE.  ) ) STOP 32
  IF ( ANY(LZ6  .NEQV. .TRUE.  ) ) STOP 33


  END

 
