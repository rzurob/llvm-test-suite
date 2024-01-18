!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TESTOP CASE NAME             : InitExpAssgn.f  
!*  TESTOP CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Sept. 07 2006
!*  ORIGIN                     : Compiler Development IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Charber 289074 
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
!*  Assignment 
!* 
!*
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpAssgn 
  IMPLICIT NONE

  INTEGER     :: I

  COMPLEX(4) , PARAMETER :: Z4(128)=  [((I,-I),I=0,127)]
  COMPLEX(8) , PARAMETER :: Z8(128)=  [((I,-I),I=0,127)]
  COMPLEX(16), PARAMETER :: Z6(128)=  [((I,-I),I=0,127)]

  INTEGER(1), PARAMETER   :: I1(128) = Z4
  INTEGER(2), PARAMETER   :: I2(128) = Z8
  INTEGER(4), PARAMETER   :: I4(128) = Z6
  INTEGER(8), PARAMETER   :: I8(128) = Z4

  REAL(4),  PARAMETER    :: R4(128) = Z8
  REAL(8),  PARAMETER    :: R8(128) = Z6
  REAL(16), PARAMETER    :: R6(128) = Z4

  COMPLEX(4)  :: TZ4(128) = I8
  COMPLEX(8)  :: TZ8(128) = I1
  COMPLEX(16) :: TZ6(128) = R8



  IF ( ANY( Z4  .NE. [((I,-I),I=0,127)]   ) ) STOP 11
  IF ( ANY( Z8  .NE. Z4   ) ) STOP 12
  IF ( ANY( Z6  .NE. Z4   ) ) STOP 13

  IF ( ANY( I1  .NE. REAL(Z4)   ) ) STOP 21
  IF ( ANY( I2  .NE. REAL(Z4)   ) ) STOP 22
  IF ( ANY( I4  .NE. REAL(Z4)   ) ) STOP 23
  IF ( ANY( I8  .NE. REAL(Z4)   ) ) STOP 24

  IF ( ANY( R4  .NE. REAL(Z4)   ) ) STOP 31
  IF ( ANY( R8  .NE. REAL(Z4)   ) ) STOP 32
  IF ( ANY( R6  .NE. REAL(Z4)   ) ) STOP 33

  IF ( ANY( TZ4 .NE. [((I,0),I=0,127)] ) ) STOP 41
  IF ( ANY( TZ8 .NE. [((I,0),I=0,127)] ) ) STOP 42
  IF ( ANY( TZ6 .NE. [((I,0),I=0,127)] ) ) STOP 43


  END 


 
