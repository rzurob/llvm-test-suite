!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpDPC.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Aug. 25, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289074 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qrealsize 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  -qdpc 
!* 
!* (324532/324766)           
!*  
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  InitExpDPC
  IMPLICIT NONE

  INTEGER  :: I 
  REAL(8)  :: R8 = -1.00000005_8
  REAL(16) :: R6 = -1.00000005_16

  REAL(8),       PARAMETER :: R8A(128)=(/( 1.00000005_8, I=0, 127)/)  
  REAL(8),       PARAMETER :: R8B(128)=(/(-0., I=0, 127)/)  
  REAL(8),       PARAMETER :: R8Arr(128) =SIGN(A=R8A, B=R8B)
  INTEGER,       PARAMETER :: R8K = R8Arr%KIND

  REAL(16),      PARAMETER :: R6A(128)=(/( 1.00000005_16, I=0, 127)/)  
  REAL(16),      PARAMETER :: R6B(128)=(/(-0., I=0, 127)/)  
  REAL(16),      PARAMETER :: R6Arr(128) =SIGN(A=R6A, B=R6B)
  INTEGER,       PARAMETER :: R6K = R6Arr%KIND

  IF ( ANY( R8Arr  .NE. R8 ))      STOP 12
! IF ( ANY( R6Arr  .NE. R6 ))      STOP 13  ! we do not support it

  IF ( R8K .NE. 8 )  STOP 14
  IF ( R6K .NE. 16)  STOP 15

  END


