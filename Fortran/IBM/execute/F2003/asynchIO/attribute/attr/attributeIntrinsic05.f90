!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : attributeIntrinsic05 - ASYNCHRONOUS
!*                               Attribute in Intrinsic Types
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January 16, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute in a Type
!*                               Declaration Statement
!*  SECONDARY FUNCTIONS TESTED : Intrinsic Type LOGICAL Declaration
!*
!*  DRIVER STANZA              : xlf2003
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
