!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM REALERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : fgeneralREAL32
!*
!*  PROGRAMMER                 : Morteza Ershad-Manesh
!*  DATE                       : 2010-07-12
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 376078
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : 
!*
!*  DESCRIPTION
!*   This test case uses REAL32 from IOS_FORTRAN_ENV module.
!*   It checks the KIND,RANGE and BIT_SIZE of REAL32
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM fgeneralREAL32
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

REAL(REAL32) :: i32

i32=1.402823E+12

PRINT*,"REAL32", i32
PRINT*,"KIND: ",KIND(i32)
PRINT*,"RANGE: ",RANGE(i32)

END PROGRAM
