!*  ===================================================================
!*
!*                               Attribute in Derived Types
!*
!*  DATE                       : January 16, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute in a Type
!*                               Declaration Statement
!*  SECONDARY FUNCTIONS TESTED : Derived Type Declaration
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To confirm that a variable defined to be of a Derived Type has the
!*  ASYNCHRONOUS Attribute when explicitly specified via the ASYNCHRONOUS
!*  Attribute in a Type Declaration Statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM attributeDerived01

    TYPE tPoint
        REAL :: x, y
        CHARACTER(LEN = 10) :: name
    END TYPE tPoint

    TYPE( tPoint ), ASYNCHRONOUS :: asynchPoint

    asynchPoint%x = 5.2
    asynchPoint%y = 1.3
    asynchPoint%name = 'alpha'

END PROGRAM attributeDerived01
