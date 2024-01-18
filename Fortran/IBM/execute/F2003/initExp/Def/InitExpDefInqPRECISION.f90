!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefInqPRECISION.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 04, 2006
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
!*  -  PRECISION 
!* 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM    InitExpDefInqPRECISION 
  IMPLICIT NONE
  INTEGER :: I, J, K

 
  REAL(4),   PARAMETER :: R4 = 10
  REAL(8),   PARAMETER :: R8(-2147483648:-2147483647, 2147483646:2147483647) = 1
  REAL(16),  PARAMETER :: R6(1:0) = -1 

  INTEGER  :: TR4(16, 16)  = RESHAPE((/(PRECISION(R4), I=1,256)/), (/16,16/))
  INTEGER  :: TR8(16, 16)  = RESHAPE((/(PRECISION(R8), I=1,256)/), (/16,16/))
  INTEGER  :: TR6(16, 16)  = RESHAPE((/(PRECISION(R6), I=1,256)/), (/16,16/))

  COMPLEX(4),   PARAMETER :: Z4 = (10,10)
  COMPLEX(8),   PARAMETER :: Z8(-2147483648:-2147483647, 2147483646:2147483647) = (1,0) 
  COMPLEX(16),  PARAMETER :: Z6(1:0) = (0,-1) 

  INTEGER  :: TZ4(16, 16)  = RESHAPE((/(PRECISION(Z4), I=1,256)/), (/16,16/))
  INTEGER  :: TZ8(16, 16)  = RESHAPE((/(PRECISION(Z8), I=1,256)/), (/16,16/))
  INTEGER  :: TZ6(16, 16)  = RESHAPE((/(PRECISION(Z6), I=1,256)/), (/16,16/))


  IF ( ANY ( TR4  .NE. 6  )  )   STOP 11
  IF ( ANY ( TR8  .NE. 15 )  )   STOP 12
  IF ( ANY ( TR6  .NE. 31 )  )   STOP 13

  IF ( ANY ( TZ4  .NE. 6  )  )   STOP 21
  IF ( ANY ( Tz8  .NE. 15 )  )   STOP 22
  IF ( ANY ( TZ6  .NE. 31 )  )   STOP 23

  END


 
