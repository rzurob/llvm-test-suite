! GB DTP extension using:
! ftcx_dtp /tstdev/F2003/asynchIO/attribute/stmt/statementDerived01.f
! opt variations: -qck

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : statementDerived01 - ASYNCHRONOUS
!*                               Attribute in Derived Types
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January 16, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Statement
!*  SECONDARY FUNCTIONS TESTED : Derived Type Declaration
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  To confirm that a variable defined to be of a Derived Type has the
!*  ASYNCHRONOUS Attribute when explicitly specified via the ASYNCHRONOUS
!*  statement.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM statementDerived01

    TYPE tPoint(K1,N1)    ! (4,10)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
        REAL(K1)      :: x, y
        CHARACTER(N1) :: name
    END TYPE tPoint

    TYPE( tPoint(4,10) ) :: asynchPoint
    ASYNCHRONOUS :: asynchPoint

    asynchPoint%x = 5.2
    asynchPoint%y = 1.3
    asynchPoint%name = 'alpha'

END PROGRAM statementDerived01
