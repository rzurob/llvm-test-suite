!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TESTOP CASE NAME             : InitExpIntDiv.f  
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
!*  REFERENCE                  : Feature Number 289074 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER IntDivTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  Integer Division 
!* 
!*  
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpIntDiv 

  INTEGER(1), PARAMETER :: A(128)=1
  INTEGER(2), PARAMETER :: B(128)=2
  INTEGER(4), PARAMETER :: C(128)=1
  INTEGER(8), PARAMETER :: D(128)=3

  INTEGER(KIND=1)  :: I1(128)= A*(C/B)
  INTEGER(KIND=2)  :: I2(128)= A*C/B
  INTEGER(KIND=4)  :: I4(128)= B**(-A) 
  INTEGER(KIND=8)  :: I8(128)= A + B**(-D) 


  IF ( ANY(I1   .NE. 0 ) ) STOP 11
  IF ( ANY(I2   .NE. 0 ) ) STOP 12
  IF ( ANY(I4   .NE. 0 ) ) STOP 13
  IF ( ANY(I8   .NE. A ) ) STOP 14


  END 


 
