!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : InitExpTypSpecVec.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Aug. 29, 2006
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
!*  vector subscript 
!* 
!*  
!* 
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpTypSpecVec 
  IMPLICIT NONE

  INTEGER :: I

  INTEGER(1),  PARAMETER :: IV1(-128:-1)=[(I, I=-128, -1)]
  INTEGER(2),  PARAMETER :: IV2(-128:-1)=[(I, I=-128, -1)]
  INTEGER(4),  PARAMETER :: IV4(-128:-1)=[(I, I=-128, -1)]
  INTEGER(8),  PARAMETER :: IV8(-128:-1)=[(I, I=-128, -1)]

  LOGICAL(1),  PARAMETER :: LV1(-128:-1)=[(.TRUE., .FALSE., I=-128, -1, 2)]
  LOGICAL(2),  PARAMETER :: LV2(-128:-1)=[(.TRUE., .FALSE., I=-128, -1, 2)]
  LOGICAL(4),  PARAMETER :: LV4(-128:-1)=[(.TRUE., .FALSE., I=-128, -1, 2)]
  LOGICAL(8),  PARAMETER :: LV8(-128:-1)=[(.TRUE., .FALSE., I=-128, -1, 2)]

  INTEGER(1),  PARAMETER :: S1(-128:-1)=[(I, I=-1,-128, -1)]
  INTEGER(2),  PARAMETER :: S2(-128:-1)=[(I, I=-1,-128, -1)]
  INTEGER(4),  PARAMETER :: S4(-128:-1)=[(I, I=-1,-128, -1)]
  INTEGER(8),  PARAMETER :: S8(-128:-1)=[(I, I=-1,-128, -1)]

  INTEGER,  PARAMETER :: IR(128)=[(I, I=-1, -128, -1)]
  LOGICAL,  PARAMETER :: LR(128)=[(.FALSE., .TRUE., I=-1, -128, -2)]

 
  INTEGER(KIND=1), PARAMETER :: I1(128)=IV8(S8)
  INTEGER(KIND=2), PARAMETER :: I2(128)=IV4(S4)
  INTEGER(KIND=4), PARAMETER :: I4(128)=IV2(S2)
  INTEGER(KIND=8), PARAMETER :: I8(128)=IV1(S1)

  LOGICAL(KIND=1), PARAMETER :: L1(128)=LV8(S8)
  LOGICAL(KIND=2), PARAMETER :: L2(128)=LV4(S4)
  LOGICAL(KIND=4), PARAMETER :: L4(128)=LV2(S2)
  LOGICAL(KIND=8), PARAMETER :: L8(128)=LV1(S1)

  IF ( KIND(I1) .NE. 1      ) STOP 11
  IF ( KIND(I2) .NE. 2      ) STOP 12
  IF ( KIND(I4) .NE. 4      ) STOP 13
  IF ( KIND(I8) .NE. 8      ) STOP 14

  IF ( ANY(I1   .NE. IR ) ) STOP 21
  IF ( ANY(I2   .NE. IR ) ) STOP 22
  IF ( ANY(I4   .NE. IR ) ) STOP 23
  IF ( ANY(I8   .NE. IR ) ) STOP 24

  IF ( KIND(L1) .NE. 1      ) STOP 31
  IF ( KIND(L2) .NE. 2      ) STOP 32
  IF ( KIND(L4) .NE. 4      ) STOP 33
  IF ( KIND(L8) .NE. 8      ) STOP 34

  IF ( ANY(L1   .NEQV. LR ) ) STOP 41
  IF ( ANY(L2   .NEQV. LR ) ) STOP 42
  IF ( ANY(L4   .NEQV. LR ) ) STOP 43
  IF ( ANY(L8   .NEQV. LR ) ) STOP 44

  END

 
