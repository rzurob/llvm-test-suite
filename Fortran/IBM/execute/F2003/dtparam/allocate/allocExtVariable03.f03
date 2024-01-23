!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : January 13, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is an Extended Derived Type
!*  SECONDARY FUNCTIONS TESTED : and the allocation-list contains a Function
!*                               Return Value
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Where allocation-list (without type-spec:: and using:  Extended Derived
!*  Types) is:
!*  o  Basic Derived Types
!*
!*  Types of allocation-list Objects:
!*  o  Function Return Value
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE baseMod

    IMPLICIT NONE

    TYPE base(l1,k)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k

        REAL(k) :: a( l1 )
    END TYPE base

END MODULE baseMod

MODULE extMod
    USE baseMod

    IMPLICIT NONE

    TYPE, EXTENDS(base) :: ext(l2)
        INTEGER, LEN :: l2

        TYPE(base((l1 * l2),k)) :: b
    END TYPE ext

END MODULE extMod


PROGRAM allocExtVariable03
    USE extMod

    IMPLICIT NONE

    INTEGER :: l1
    INTEGER :: l2

    TYPE(ext(5,16,1)), POINTER  :: e1
    TYPE(ext(0,16,1)), POINTER  :: e2
    TYPE(ext(5,16,0)), POINTER  :: e3


    IF ( ASSOCIATED( e1 ) )                 ERROR STOP 10_4

    l1 = 5
    l2 = 1

    e1 => NewExt( 31_4 )

    IF (.NOT. ASSOCIATED( e1 ))             ERROR STOP 12_4

    IF (e1%k /= 16)                         ERROR STOP 13_4
    IF (e1%b%k /= 16)                       ERROR STOP 14_4
    IF (e1%base%k /= 16)                    ERROR STOP 15_4
    IF (KIND( e1%b%a ) /= 16)               ERROR STOP 16_4
    IF (KIND( e1%base%a ) /= 16)            ERROR STOP 17_4

    IF (e1%l2 /= 1)                         ERROR STOP 18_4
    IF (e1%l1 /= 5)                         ERROR STOP 19_4
    IF (e1%b%l1 /= 5)                       ERROR STOP 20_4
    IF (e1%base%l1 /= 5)                    ERROR STOP 21_4

    IF (SIZE( e1%b%a ) /= 5)                ERROR STOP 22_4
    IF (SIZE( e1%base%a ) /= 5)             ERROR STOP 23_4


    IF ( ASSOCIATED( e2 ) )                 ERROR STOP 30_4

    l1 = 0
    l2 = 1

    e2 => NewExt( 31_4 )

    IF (.NOT. ASSOCIATED( e2 ))             ERROR STOP 32_4

    IF (e2%k /= 16)                         ERROR STOP 33_4
    IF (e2%b%k /= 16)                       ERROR STOP 34_4
    IF (e2%base%k /= 16)                    ERROR STOP 35_4
    IF (KIND( e2%b%a ) /= 16)               ERROR STOP 36_4
    IF (KIND( e2%base%a ) /= 16)            ERROR STOP 37_4

    IF (e2%l2 /= 1)                         ERROR STOP 38_4
    IF (e2%l1 /= 0)                         ERROR STOP 39_4
    IF (e2%b%l1 /= 0)                       ERROR STOP 40_4
    IF (e2%base%l1 /= 0)                    ERROR STOP 41_4

    IF (SIZE( e2%b%a ) /= 0)                ERROR STOP 42_4
    IF (SIZE( e2%base%a ) /= 0)             ERROR STOP 43_4


    IF ( ASSOCIATED( e3 ) )                 ERROR STOP 50_4

    l1 = 5
    l2 = 0

    e3 => NewExt( 51_4 )

    IF (.NOT. ASSOCIATED( e3 ))             ERROR STOP 52_4

    IF (e3%k /= 16)                         ERROR STOP 53_4
    IF (e3%b%k /= 16)                       ERROR STOP 54_4
    IF (e3%base%k /= 16)                    ERROR STOP 55_4
    IF (KIND( e3%b%a ) /= 16)               ERROR STOP 56_4
    IF (KIND( e3%base%a ) /= 16)            ERROR STOP 57_4

    IF (e3%l2 /= 0)                         ERROR STOP 58_4
    IF (e3%l1 /= 5)                         ERROR STOP 59_4
    IF (e3%b%l1 /= 0)                       ERROR STOP 60_4
    IF (e3%base%l1 /= 5)                    ERROR STOP 61_4

    IF (SIZE( e3%b%a ) /= 0)                ERROR STOP 62_4
    IF (SIZE( e3%base%a ) /= 5)             ERROR STOP 63_4


    CONTAINS

        FUNCTION NewExt( rc )
            INTEGER(4) :: rc

            TYPE(ext(l1,16,l2)), POINTER  :: NewExt

            INTEGER :: stat
            CHARACTER(255) :: errmsg

            ALLOCATE(NewExt, STAT=stat, ERRMSG=errmsg)
            IF (stat /= 0) THEN
                PRINT *, 'ALLOCATE(NewExt,STAT=', stat, ') ', errmsg
                CALL zzrc( rc )
            END IF

        END FUNCTION NewExt


END PROGRAM allocExtVariable03
