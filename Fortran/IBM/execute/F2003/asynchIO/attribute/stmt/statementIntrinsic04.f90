!*  ===================================================================
!*
!*                               Attribute in Intrinsic Types
!*
!*  DATE                       : January 16, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Statement
!*  SECONDARY FUNCTIONS TESTED : Intrinsic Type CHARACTER Declaration
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To confirm that a variable defined to be of the Intrinsic Type
!*  CHARACTER has the ASYNCHRONOUS Attribute when explicitly specified
!*  via the ASYNCHRONOUS statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM statementIntrinsic04
    IMPLICIT NONE

    CHARACTER(LEN = 13) :: asynchCharacter
    ASYNCHRONOUS asynchCharacter

    asynchCharacter = 'Hello, World!'

END PROGRAM statementIntrinsic04
