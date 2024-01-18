!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefSpecBIT_SIZE.f  
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
!*  - BIT_SIZE 
!* 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpDefSpecBIT_SIZE 
  IMPLICIT NONE
  INTEGER :: I, J, K

  INTEGER(1),  PARAMETER :: I1(-2147483648:-2147483647, 2147483646:2147483647) = -1 
  INTEGER(2),  PARAMETER :: I2 = -1 
  INTEGER(4),  PARAMETER :: I4(-2147483648:-2147483647, 2147483646:2147483647) = -1 
  INTEGER(8),  PARAMETER :: I8 = -1 

  INTEGER(KIND(I1))  :: TI1 = BIT_SIZE(I1)
  INTEGER(KIND(I2))  :: TI2 = BIT_SIZE(I2)
  INTEGER(KIND(I4))  :: TI4 = BIT_SIZE(I4)
  INTEGER(KIND(I8))  :: TI8 = BIT_SIZE(I8)

  LOGICAL(1),  PARAMETER :: L1(-2147483648:-2147483647, 2147483646:2147483647) = -1 
  LOGICAL(2),  PARAMETER :: L2 = -1 
  LOGICAL(4),  PARAMETER :: L4(-2147483648:-2147483647, 2147483646:2147483647) = -1 
  LOGICAL(8),  PARAMETER :: L8 = -1 

  LOGICAL(KIND(L1))  :: TL1 = BIT_SIZE(L1)
  LOGICAL(KIND(L2))  :: TL2 = BIT_SIZE(L2)
  LOGICAL(KIND(L4))  :: TL4 = BIT_SIZE(L4)
  LOGICAL(KIND(L8))  :: TL8 = BIT_SIZE(L8)

  IF ( TI1     .NE. KIND(TI1) * 8  )            STOP 11
  IF ( TI2     .NE. KIND(TI2) * 8  )            STOP 12
  IF ( TI4     .NE. KIND(TI4) * 8  )            STOP 14
  IF ( TI8     .NE. KIND(TI8) * 8  )            STOP 18

  IF ( TL1     .NE. KIND(TI1) * 8  )            STOP 21
  IF ( TL2     .NE. KIND(TI2) * 8  )            STOP 22
  IF ( TL4     .NE. KIND(TI4) * 8  )            STOP 24
  IF ( TL8     .NE. KIND(TI8) * 8  )            STOP 28


  END


 
