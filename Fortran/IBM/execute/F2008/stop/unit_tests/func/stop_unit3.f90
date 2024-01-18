!*  ===================================================================
!*
!*  TEST CASE NAME             : stop_unit3.f
!*
!*  DATE                       : Sept 29, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Unit test for STOP statement
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      INTEGER, PARAMETER :: STOP_BASE = 10
      STOP STOP_BASE + 20
      END

