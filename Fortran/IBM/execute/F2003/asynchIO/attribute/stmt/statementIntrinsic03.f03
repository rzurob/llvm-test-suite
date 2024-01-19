!*  ===================================================================
!*
!*                               Attribute in Intrinsic Types
!*
!*  DATE                       : January 16, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Statement
!*  SECONDARY FUNCTIONS TESTED : Intrinsic Type COMPLEX Declaration
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To confirm that a variable defined to be of the Intrinsic Type
!*  COMPLEX has the ASYNCHRONOUS Attribute when explicitly specified
!*  via the ASYNCHRONOUS statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM statementIntrinsic03
    IMPLICIT NONE

    COMPLEX :: asynchComplex
    ASYNCHRONOUS asynchComplex

    asynchComplex = ( 5.0,3.0 )

END PROGRAM statementIntrinsic03
