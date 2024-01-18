!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : fgeneralREAL128
!*
!*  DATE                       : 2010-07-12
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 376078
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION
!*   This test case uses REAL128 from IOS_FORTRAN_ENV module.
!*   It checks the KIND,RANGE and BIT_SIZE of REAL128
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM fgeneralREAL128
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

REAL(REAL128) :: i128

i128=1.797693Q+307

PRINT*,"REAL128", i128
PRINT*,"KIND: ",KIND(i128)
PRINT*,"RANGE: ",RANGE(i128)

END PROGRAM
