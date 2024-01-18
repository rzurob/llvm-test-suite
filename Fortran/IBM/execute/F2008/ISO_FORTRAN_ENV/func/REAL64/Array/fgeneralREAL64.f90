!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM REALERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : fgeneralREAL64
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
!*   This test case uses REAL64 from IOS_FORTRAN_ENV module.
!*   It checks the KIND,RANGE and BIT_SIZE of REAL64
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM fgeneralREAL64
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none 

REAL(REAL64) :: i64

i64=1.797693D+208

PRINT*,"REAL64", i64
PRINT*,"KIND: ",KIND(i64)
PRINT*,"RANGE: ",RANGE(i64)

END PROGRAM
