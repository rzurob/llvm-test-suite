!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : C507Derived01d - ASYNCHRONOUS
!*                               Attribute in Derived Types
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January 17, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C507
!*  SECONDARY FUNCTIONS TESTED : Derived Type -- Base Object
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

PROGRAM C507Derived01d

    TYPE tPoint
        REAL :: x, y
        CHARACTER(LEN = 10) :: name
    END TYPE tPoint

    TYPE( tPoint ), ASYNCHRONOUS, ASYNCHRONOUS :: asynchPoint

    asynchPoint%x = 5.2
    asynchPoint%y = 1.3
    asynchPoint%name = 'alpha'

END PROGRAM C507Derived01d
