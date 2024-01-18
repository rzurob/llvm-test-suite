!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpIntrinOP1.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Aug. 29 2006
!*  ORIGIN                     : Compiler Development IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289074 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OP1TIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  Characteristics of intrinsic op on entities of zero size 
!* 
!*  (324911/325918/328083) 
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpIntrinOP1 

  INTEGER(KIND=1)  :: I1(128)=KIND([INTEGER(1)::])
  INTEGER(KIND=2)  :: I2(128)=KIND(0_2 + I1(1:0))
  INTEGER(KIND=4)  :: I4(128)=KIND(I2(1:0) + 0_4 )
  INTEGER(KIND=8)  :: I8(128)=KIND(0_4 + I4(1:0) + 0_8)

  LOGICAL(KIND=1)  :: L1(128)=.FALSE._1 
  LOGICAL(KIND=2)  :: L2(128)=.FALSE._2
  LOGICAL(KIND=4)  :: L4(128)=.FALSE._4
  LOGICAL(KIND=8)  :: L8(128)=.FALSE._8

  INTEGER(KIND=1)  :: IL1(128)=KIND([LOGICAL(1) ::]) 
  INTEGER(KIND=2)  :: IL2(128)=KIND(.TRUE._1  .OR. L1(1:0))
  INTEGER(KIND=4)  :: IL4(128)=KIND( L2(1:0)  .OR. .TRUE._1 .OR. .FALSE._4 )
  INTEGER(KIND=8)  :: IL8(128)=KIND(.FALSE._1 .OR. .TRUE._2 .OR. L8(1:0))

  INTEGER :: R4(128)= KIND(I1(1:0) + 0._4)
  INTEGER :: R8(128)= KIND(0._8 + R4(1:0))
  INTEGER :: R6(128)= KIND(0._16 + 1_8 + R8(1:0))

  INTEGER :: Z4(128)= KIND([(0._4,0._4)]  + 0._8)
  INTEGER :: Z8(128)= KIND(0._8 + Z4(1:0))
  INTEGER :: Z6(128)= KIND(R8(1:0) + (0._4, 0._4) + 0._16 )

 

  IF ( ANY(I1   .NE. 1 ) ) STOP 11
  IF ( ANY(I2   .NE. 2 ) ) STOP 12
  IF ( ANY(I4   .NE. 4 ) ) STOP 13
  IF ( ANY(I8   .NE. 8 ) ) STOP 14

  IF ( ANY(IL1   .NE. 1 ) ) STOP 21
  IF ( ANY(IL2   .NE. 1 ) ) STOP 22
  IF ( ANY(IL4   .NE. 4 ) ) STOP 23
  IF ( ANY(IL8   .NE. 8 ) ) STOP 24

  IF ( ANY(R4   .NE. 4 ) ) STOP 31
  IF ( ANY(R8   .NE. 8 ) ) STOP 32
  IF ( ANY(R6   .NE. 16) ) STOP 33

  IF ( ANY(Z4   .NE. 8 ) ) STOP 41
  IF ( ANY(Z8   .NE. 8 ) ) STOP 42
  IF ( ANY(Z6   .NE. 16) ) STOP 43


  END 


 
