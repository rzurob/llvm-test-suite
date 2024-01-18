!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : fgeneralINT64
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
!*   This test case uses INT64 from IOS_FORTRAN_ENV module.
!*   It checks the KIND,RANGE and BIT_SIZE of INT64
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM fgeneralINT64
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

INTEGER(INT64) :: i64

i64=3423423044364

PRINT*,"INT64", i64
PRINT*,"KIND: ",KIND(i64)
PRINT*,"RANGE: ",RANGE(i64)
PRINT*,"BIT SIZE: ",BIT_SIZE(i64)

END PROGRAM
