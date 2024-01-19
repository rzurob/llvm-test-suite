!*********************************************************************
!*  ===================================================================
!*
!*  TESTOP CASE NAME             : InitExpNumRelOP.f
!*  TESTOP CASE TITLE            :
!*
!*  DATE                       : Sept. 07 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Fortran 2003 Initialization Expression Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289074
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!* Numeric relational operation
!*
!* TYPE parameter convered to that of x + y before eval
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  PROGRAM InitExpNumRelOP
  IMPLICIT NONE

  INTEGER     :: I

  REAL(4),  PARAMETER :: R4(128) = 3._4
  REAL(8),  PARAMETER :: R8(128) = 3._8
  REAL(16), PARAMETER :: R6(128) = 3._16

  LOGICAL :: L1(128) = 1/R4 < 1/R8  .EQV. REAL(1/R4, 8)  < 1/R8
  LOGICAL :: L2(128) = 1/R4 < 1/R6  .EQV. REAL(1/R4, 16) < 1/R6
  LOGICAL :: L3(128) = 1/R8 < 1/R6  .EQV. REAL(1/R8, 16) < 1/R6

  LOGICAL :: L4(128) = 1/R4 <= 1/R8  .EQV. REAL(1/R4, 8)  <= 1/R8
  LOGICAL :: L5(128) = 1/R4 <= 1/R6  .EQV. REAL(1/R4, 16) <= 1/R6
  LOGICAL :: L6(128) = 1/R8 <= 1/R6  .EQV. REAL(1/R8, 16) <= 1/R6

  LOGICAL :: L7(128) = 1/R4 > 1/R8  .EQV. REAL(1/R4, 8)  > 1/R8
  LOGICAL :: L8(128) = 1/R4 > 1/R6  .EQV. REAL(1/R4, 16) > 1/R6
  LOGICAL :: L9(128) = 1/R8 > 1/R6  .EQV. REAL(1/R8, 16) > 1/R6

  LOGICAL :: L11(128) = 1/R4 >= 1/R8  .EQV. REAL(1/R4, 8)  >= 1/R8
  LOGICAL :: L12(128) = 1/R4 >= 1/R6  .EQV. REAL(1/R4, 16) >= 1/R6
  LOGICAL :: L13(128) = 1/R8 >= 1/R6  .EQV. REAL(1/R8, 16) >= 1/R6

  LOGICAL :: L14(128) = 1/R4 == 1/R8  .EQV. REAL(1/R4, 8)  == 1/R8
  LOGICAL :: L15(128) = 1/R4 == 1/R6  .EQV. REAL(1/R4, 16) == 1/R6
  LOGICAL :: L16(128) = 1/R8 == 1/R6  .EQV. REAL(1/R8, 16) == 1/R6

  LOGICAL :: L17(128) = 1/R4 /= 1/R8  .EQV. REAL(1/R4, 8)  /= 1/R8
  LOGICAL :: L18(128) = 1/R4 /= 1/R6  .EQV. REAL(1/R4, 16) /= 1/R6
  LOGICAL :: L19(128) = 1/R8 /= 1/R6  .EQV. REAL(1/R8, 16) /= 1/R6


  IF ( ANY( .NOT. L1  ) ) ERROR STOP 11
  IF ( ANY( .NOT. L2  ) ) ERROR STOP 12
  IF ( ANY( .NOT. L3  ) ) ERROR STOP 13
  IF ( ANY( .NOT. L4  ) ) ERROR STOP 14
  IF ( ANY( .NOT. L5  ) ) ERROR STOP 15
  IF ( ANY( .NOT. L6  ) ) ERROR STOP 16
  IF ( ANY( .NOT. L7  ) ) ERROR STOP 17
  IF ( ANY( .NOT. L8  ) ) ERROR STOP 18
  IF ( ANY( .NOT. L9  ) ) ERROR STOP 19

  IF ( ANY( .NOT. L11  ) ) ERROR STOP 111
  IF ( ANY( .NOT. L12  ) ) ERROR STOP 112
  IF ( ANY( .NOT. L13  ) ) ERROR STOP 113
  IF ( ANY( .NOT. L14  ) ) ERROR STOP 114
  IF ( ANY( .NOT. L15  ) ) ERROR STOP 115
  IF ( ANY( .NOT. L16  ) ) ERROR STOP 116
  IF ( ANY( .NOT. L17  ) ) ERROR STOP 117
  IF ( ANY( .NOT. L18  ) ) ERROR STOP 118
  IF ( ANY( .NOT. L19  ) ) ERROR STOP 119




  END



