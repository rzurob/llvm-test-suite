!*******************************************************************************
!*  ============================================================================
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
!*   This test case uses INT8 from IOS_FORTRAN_ENV module.
!*   It checks the KIND,RANGE and BIT_SIZE of INT8
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM fgeneralINT8
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

INTEGER(INT8) :: i8

i8=126

PRINT*,"INT8", i8
PRINT*,"KIND: ",KIND(i8)
PRINT*,"RANGE: ",RANGE(i8)
PRINT*,"BIT SIZE: ",BIT_SIZE(i8)

END PROGRAM
