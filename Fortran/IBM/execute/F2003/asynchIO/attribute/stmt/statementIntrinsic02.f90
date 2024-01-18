!*  ===================================================================
!*
!*                               Attribute in Intrinsic Types
!*
!*  DATE                       : January 16, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Statement
!*  SECONDARY FUNCTIONS TESTED : Intrinsic Type REAL Declaration
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To confirm that a variable defined to be of the Intrinsic Type
!*  REAL has the ASYNCHRONOUS Attribute when explicitly specified
!*  via the ASYNCHRONOUS statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM statementIntrinsic02
    IMPLICIT NONE

    REAL :: asynchReal
    ASYNCHRONOUS asynchReal

    asynchReal = 5.0

END PROGRAM statementIntrinsic02
