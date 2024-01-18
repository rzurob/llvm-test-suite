!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefInqHUGE.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 03, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289074 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  a reference to a specification inquiry 
!* 
!*  - HUGE 
!* 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM         InitExpDefInqHUGE 
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(1),  PARAMETER :: I1(1:0) = -1 
  INTEGER(2),  PARAMETER :: I2(1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1) = -1 
  INTEGER(4),  PARAMETER :: I4 = 10
  INTEGER(8),  PARAMETER :: I8(-2147483648:-2147483647, 2147483646:2147483647) = 1

  INTEGER(KIND(HUGE(I1))):: TI1 = HUGE(I1)
  INTEGER(KIND(HUGE(I2))):: TI2 = HUGE(I2)
  INTEGER(KIND(HUGE(I4))):: TI4 = HUGE(I4)
  INTEGER(KIND(HUGE(I8))):: TI8 = HUGE(I8)
 
  INTEGER(KIND(HUGE(TI1))), PARAMETER :: MI1 = 2_1**DIGITS(I1)-1
  INTEGER(KIND(HUGE(TI2))), PARAMETER :: MI2 = 2_2**DIGITS(I2)-1
  INTEGER(KIND(HUGE(TI4))), PARAMETER :: MI4 = 2_4**DIGITS(I4)-1
  INTEGER(KIND(HUGE(TI8))), PARAMETER :: MI8 = 2_8**DIGITS(I8)-1
 
 
  REAL(4),   PARAMETER :: R4 = 10
  REAL(8),   PARAMETER :: R8(-2147483648:-2147483647, 2147483646:2147483647) = 1
  REAL(16),  PARAMETER :: R6(1:0) = -1 

  REAL(KIND(HUGE(R4)))  :: TR4  = HUGE(R4)
  REAL(KIND(HUGE(R8)))  :: TR8  = HUGE(R8)
  REAL(KIND(HUGE(R6)))  :: TR6  = HUGE(R6)

  IF ( KIND(TI1)    .NE. 1     )     STOP 11
  IF ( TI1          .NE. MI1     )   STOP 12
  IF ( KIND(TI2)    .NE. 2     )     STOP 13
  IF ( TI2          .NE. MI2     )   STOP 14
  IF ( KIND(TI4)    .NE. 4     )     STOP 15
  IF ( TI4          .NE. MI4     )   STOP 16
  IF ( KIND(TI8)    .NE. 8     )     STOP 17
  IF ( TI8          .NE. MI8     )   STOP 18

  IF ( KIND(TR4)    .NE. 4     )     STOP 21
  IF ( TR4          .NE. HUGE(R4))   STOP 22
  IF ( KIND(TR8)    .NE. 8     )     STOP 23
  IF ( TR8          .NE. HUGE(R8))   STOP 24
  IF ( KIND(TR6)    .NE. 16     )    STOP 25
  IF ( TR6          .NE. HUGE(R6))   STOP 26

  END


 
