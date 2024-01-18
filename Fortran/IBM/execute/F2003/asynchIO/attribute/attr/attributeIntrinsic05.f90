!*  ===================================================================
!*
!*                               Attribute in Intrinsic Types
!*
!*  DATE                       : January 16, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute in a Type
!*                               Declaration Statement
!*  SECONDARY FUNCTIONS TESTED : Intrinsic Type LOGICAL Declaration
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To confirm that a variable defined to be of the Intrinsic Type
!*  LOGICAL has the ASYNCHRONOUS Attribute when explicitly specified
!*  via the ASYNCHRONOUS Attribute in a Type Declaration Statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM attributeIntrinsic05
    IMPLICIT NONE

    LOGICAL, ASYNCHRONOUS :: asynchLogical

    asynchLogical = .TRUE.

END PROGRAM attributeIntrinsic05
