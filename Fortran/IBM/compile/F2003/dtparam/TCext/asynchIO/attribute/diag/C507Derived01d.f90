! GB DTP extension using:
! ftcx_dtp /tstdev/F2003/asynchIO/attribute/diag/C507Derived01d.f
! opt variations: -qck

!*  ===================================================================
!*
!*                               Attribute in Derived Types
!*
!*  DATE                       : January 17, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : C507
!*  SECONDARY FUNCTIONS TESTED : Derived Type -- Base Object
!*
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

    TYPE tPoint(K1,N1)    ! (4,10)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
        REAL(K1)      :: x, y
        CHARACTER(N1) :: name
    END TYPE tPoint

    TYPE( tPoint(4,10) ), ASYNCHRONOUS, ASYNCHRONOUS :: asynchPoint

    asynchPoint%x = 5.2
    asynchPoint%y = 1.3
    asynchPoint%name = 'alpha'

END PROGRAM C507Derived01d
