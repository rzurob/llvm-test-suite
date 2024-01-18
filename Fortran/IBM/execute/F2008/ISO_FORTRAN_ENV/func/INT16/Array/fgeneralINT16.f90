!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : fgeneralINT16
!*
!*  DATE                       : 2010-07-12
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION
!*   This test case uses INT16 from IOS_FORTRAN_ENV module.
!*   It checks the KIND,RANGE and BIT_SIZE of INT16
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM fgeneralINT16
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

INTEGER(INT16) :: i16

i16=1000

PRINT*,"INT16", i16
PRINT*,"KIND: ",KIND(i16)
PRINT*,"RANGE: ",RANGE(i16)
PRINT*,"BIT SIZE: ",BIT_SIZE(i16)

END PROGRAM
