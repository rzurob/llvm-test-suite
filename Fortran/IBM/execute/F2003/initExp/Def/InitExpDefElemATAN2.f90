!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDefElemATAN2.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar 24, 2006
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
!*  a reference to an elemental intrinsic
!* 
!*  -ATAN2
!*  (318946)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InitExpDefElemATAN2 
  IMPLICIT NONE
  INTEGER :: I, J, K
  REAL(4), PARAMETER    :: Y4(8)=1.5574077
  REAL(8), PARAMETER    :: Y8(8)=1.5574077
  REAL(16), PARAMETER   :: Y16(8)=1.5574077
  REAL(8), PARAMETER   :: Zero8(8)=-0.0
  REAL(4), PARAMETER   :: Zero4p(8)=0.0
  REAL(4), PARAMETER   :: Zero4n(8)=-0.0


  REAL(4) :: Res4(8)=ATAN2(Y4, 1.0_4) + ATAN2(Y4, 1.0_4)
  REAL(8) :: Res8(8)=ATAN2(Y8, 1.0_8) + ATAN2(Y8, 1.0_8)
  REAL(16):: Res16(8)=ATAN2(Y16, 1.0_16) + ATAN2(Y16, 1.0_16)

  REAL(16):: T(8) =ATAN2(3.1415926_16, -1.0_16) 
  REAL(8 ):: T1(8)=ATAN2(Zero8, 1.0_8) 
  REAL(4 ):: T2p(8)=ATAN2(Zero4p, -1.0_4) 
  REAL(4 ):: T2n(8)=ATAN2(Zero4n, -1.0_4) 

  REAL(16):: T3(8) =ATAN2(-1.0_16, -1.0_16) 

  REAL(4) :: T4(8) =ATAN2(-1.0_4, Zero4n) 

  IF (ANY(ABS(Res4 - 2.0)  .GE. 1.0e-5 ) )     STOP 11 
  IF (ANY(ABS(Res8 - 2.0)  .GE. 1.0e-5 ) )     STOP 12 
  IF (ANY(ABS(Res16 - 2.0) .GE. 1.0e-5 ) )     STOP 13 

  IF (ANY( T  .LT. 0.0 ) )                      STOP 21 
  IF (ANY( T1 .NE. Zero8 ) )                    STOP 22 

  IF (ANY( ABS(T2p-3.1415926)  .GE. 1.0e-5  ) )  STOP 31 
  IF (ANY( ABS(T2n+3.1415926)  .GE. 1.0e-5  ) )  STOP 41 

  IF (ANY( T3                     .GE. 0.0  ) )       STOP 51 
  IF (ANY( ABS(T4)-3.1415926/2.0  .GE. 1.0e-5  ) )    STOP 61 

  END

 
