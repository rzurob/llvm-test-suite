!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM LEADZERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpLEADZ.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Aug. 24, 2006
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
!* 
!* 
!* LEADZ -- An IBM extension 
!*  
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpLEADZ
  IMPLICIT NONE

  INTEGER :: I, J

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8


  INTEGER(KIND(LEADZ((/0_1,(2_1**I1, I1=0,7)/))))  :: IR1(9)  = LEADZ((/0_1,(2_1**I1, I1=0,7)/))
  INTEGER(KIND(LEADZ((/0_2,(2_2**I2, I2=0,15)/)))) :: IR2(17) = LEADZ((/0_2,(2_2**I2, I2=0,15)/))
  INTEGER(KIND(LEADZ((/0_4,(2_4**I4, I4=0,31)/)))) :: IR4(33) = LEADZ((/0_4,(2_4**I4, I4=0,31)/))
  INTEGER(KIND(LEADZ((/0_8,(2_8**I8, I8=0,63)/)))) :: IR8(65) = LEADZ((/0_8,(2_8**I8, I8=0,63)/))


  IF ( KIND(IR1)   .NE.  1 )                    STOP 11
  IF ( ANY( IR1    .NE.  (/(I,I=8,0,-1)/)) )    STOP 12
 
  IF ( KIND(IR2)   .NE.  2 )                    STOP 21
  IF ( ANY( IR2    .NE.  (/(I,I=16,0,-1)/)) )   STOP 22
 
  IF ( KIND(IR4)   .NE.  4 )                    STOP 41
  IF ( ANY( IR4    .NE.  (/(I,I=32,0,-1)/)) )   STOP 42
 
  IF ( KIND(IR8)   .NE.  8 )                    STOP 81
  IF ( ANY( IR8    .NE.  (/(I,I=64,0,-1)/)) )   STOP 82
 
 

  END


