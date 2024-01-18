!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : R521StructComp01d - ASYNCHRONOUS
!*                               Attribute in Derived Types
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January 17, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : R521
!*  SECONDARY FUNCTIONS TESTED : Derived Type -- Structure Component
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  A Structure Component cannot be specified as having the ASYNCHRONOUS
!*  Attribute using the ASYNCHRONOUS Statement:
!*
!*  5.2.3  ASYNCHRONOUS Statement
!*
!*  R521 asynchronous-stmt is ASYNCHRONOUS [ :: ] object-name-list
!*
!*
!*  5.1  Type Declaration Statements
!*
!*  R505 object-name is name
!*
!*
!*  3.2.1  Names
!*
!*  R304 name is letter [ alphanumeric-character ] ...
!*
!*
!*  3.1  Processor Character Set
!*
!*  R302 alphanumeric-character is letter
!*                              or digit
!*                              or underscore
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM R521StructComp01d

    TYPE tPoint
        REAL :: x, y
        CHARACTER(LEN = 10) :: name
    END TYPE tPoint

    TYPE( tPoint ) :: asynchPoint
    ASYNCHRONOUS :: asynchPoint%x

    asynchPoint%x = 5.2
    asynchPoint%y = 1.3
    asynchPoint%name = 'alpha'

END PROGRAM R521StructComp01d
