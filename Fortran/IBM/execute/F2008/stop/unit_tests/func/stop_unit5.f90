!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : stop_unit5.f
!*  TEST CASE TITLE            : integer expression as stop code
!*
!*  PROGRAMMER                 : Tarique Islam
!*  DATE                       : Sept 29, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Unit test for STOP statement 
!*                             :
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      INTEGER, PARAMETER :: STOP_BASE = 10 
      STOP STOP_BASE + 256
      END

