!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : C507Intrinsic01d - ASYNCHRONOUS
!*                               Attribute in Intrinsic Types
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January 17, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C507
!*  SECONDARY FUNCTIONS TESTED : Intrinsic Type
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  5.1  Type Declaration Statements
!*
!*  C507 (R501) The same attr-spec shall not appear more than once in a
!*              given type-declaration-stmt.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM C507Intrinsic01d
    IMPLICIT NONE

    LOGICAL, ASYNCHRONOUS, ASYNCHRONOUS :: asynchLogical

    asynchLogical = .TRUE.

END PROGRAM C507Intrinsic01d
