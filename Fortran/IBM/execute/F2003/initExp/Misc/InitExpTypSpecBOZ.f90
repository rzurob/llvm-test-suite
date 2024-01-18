!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpTypSpecBOZ.f  
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
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  BOZ in data stmt 
!* 
!*  
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypSpecBOZ 
  IMPLICIT NONE

  INTEGER :: I

  
  INTEGER(KIND=1_8)              :: I1(128)
  INTEGER(KIND=I1%KIND+1)        :: I2(128)
  INTEGER(KIND=2*I2%KIND)        :: I4(128)
  INTEGER(KIND=I4%KIND+I4%KIND)  :: I8(128)

  LOGICAL(KIND=I1%KIND)          :: L1(128)
  LOGICAL(KIND=L1%KIND+1)        :: L2(128)
  LOGICAL(KIND=2*L2%KIND)        :: L4(128)
  LOGICAL(KIND=L4%KIND+L4%KIND)  :: L8(128)

  DATA (I1(I), I=1, 128) /128*Z"01"/
  DATA (I2(I), I=1, 128) /128*Z"02"/
  DATA (I4(I), I=1, 128) /128*Z"04"/
  DATA (I8(I), I=1, 128) /128*Z"08"/

  DATA (L1(I), I=1, 128) /128*.TRUE._1/
  DATA (L2(I), I=1, 128) /128*.TRUE._2/
  DATA (L4(I), I=1, 128) /128*.TRUE._4/
  DATA (L8(I), I=1, 128) /128*.TRUE._8/

  IF ( KIND(I1) .NE. 1      ) STOP 11
  IF ( KIND(I2) .NE. 2      ) STOP 12
  IF ( KIND(I4) .NE. 4      ) STOP 13
  IF ( KIND(I8) .NE. 8      ) STOP 14

  IF ( ANY(I1   .NE. 1 ) ) STOP 21
  IF ( ANY(I2   .NE. 2 ) ) STOP 22
  IF ( ANY(I4   .NE. 4 ) ) STOP 23
  IF ( ANY(I8   .NE. 8 ) ) STOP 24

  IF ( KIND(L1) .NE. 1      ) STOP 31
  IF ( KIND(L2) .NE. 2      ) STOP 32
  IF ( KIND(L4) .NE. 4      ) STOP 33
  IF ( KIND(L8) .NE. 8      ) STOP 34

  IF ( ANY(L1   .NEQV. .TRUE. ) ) STOP 41
  IF ( ANY(L2   .NEQV. .TRUE. ) ) STOP 42
  IF ( ANY(L4   .NEQV. .TRUE. ) ) STOP 43
  IF ( ANY(L8   .NEQV. .TRUE. ) ) STOP 44

  END

 
