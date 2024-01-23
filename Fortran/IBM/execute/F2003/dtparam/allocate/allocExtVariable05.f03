!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : January 13, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is an Extended Derived Type
!*  SECONDARY FUNCTIONS TESTED : and the allocation-list contains a Dummy
!*                               Argument
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
!*  o  Dummy Arguments
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


PROGRAM allocExtVariable05
    USE extMod

    IMPLICIT NONE

    INTEGER :: stat

    TYPE(ext(5,8,1)), ALLOCATABLE :: e1
    TYPE(ext(0,8,1)), ALLOCATABLE :: e2
    TYPE(ext(5,8,0)), ALLOCATABLE :: e3


    IF ( ALLOCATED( e1 ) )                  ERROR STOP 10_4

    stat = NewExt( e1 )
    IF (stat /= 0)                          ERROR STOP 11_4

    IF (.NOT. ALLOCATED( e1 ))              ERROR STOP 12_4

    IF (e1%k /= 8)                          ERROR STOP 13_4
    IF (e1%b%k /= 8)                        ERROR STOP 14_4
    IF (e1%base%k /= 8)                     ERROR STOP 15_4
    IF (KIND( e1%b%a ) /= 8)                ERROR STOP 16_4
    IF (KIND( e1%base%a ) /= 8)             ERROR STOP 17_4

    IF (e1%l2 /= 1)                         ERROR STOP 18_4
    IF (e1%l1 /= 5)                         ERROR STOP 19_4
    IF (e1%b%l1 /= 5)                       ERROR STOP 20_4
    IF (e1%base%l1 /= 5)                    ERROR STOP 21_4

    IF (SIZE( e1%b%a ) /= 5)                ERROR STOP 22_4
    IF (SIZE( e1%base%a ) /= 5)             ERROR STOP 23_4


    IF ( ALLOCATED( e2 ) )                  ERROR STOP 30_4

    stat = NewExt( e2 )
    IF (stat /= 0)                          ERROR STOP 31_4

    IF (.NOT. ALLOCATED( e2 ))              ERROR STOP 32_4

    IF (e2%k /= 8)                          ERROR STOP 33_4
    IF (e2%b%k /= 8)                        ERROR STOP 34_4
    IF (e2%base%k /= 8)                     ERROR STOP 35_4
    IF (KIND( e2%b%a ) /= 8)                ERROR STOP 36_4
    IF (KIND( e2%base%a ) /= 8)             ERROR STOP 37_4

    IF (e2%l2 /= 1)                         ERROR STOP 38_4
    IF (e2%l1 /= 0)                         ERROR STOP 39_4
    IF (e2%b%l1 /= 0)                       ERROR STOP 40_4
    IF (e2%base%l1 /= 0)                    ERROR STOP 41_4

    IF (SIZE( e2%b%a ) /= 0)                ERROR STOP 42_4
    IF (SIZE( e2%base%a ) /= 0)             ERROR STOP 43_4


    IF ( ALLOCATED( e3 ) )                  ERROR STOP 50_4

    stat = NewExt( e3 )
    IF (stat /= 0)                          ERROR STOP 51_4

    IF (.NOT. ALLOCATED( e3 ))              ERROR STOP 52_4

    IF (e3%k /= 8)                          ERROR STOP 53_4
    IF (e3%b%k /= 8)                        ERROR STOP 54_4
    IF (e3%base%k /= 8)                     ERROR STOP 55_4
    IF (KIND( e3%b%a ) /= 8)                ERROR STOP 56_4
    IF (KIND( e3%base%a ) /= 8)             ERROR STOP 57_4

    IF (e3%l2 /= 0)                         ERROR STOP 58_4
    IF (e3%l1 /= 5)                         ERROR STOP 59_4
    IF (e3%b%l1 /= 0)                       ERROR STOP 60_4
    IF (e3%base%l1 /= 5)                    ERROR STOP 61_4

    IF (SIZE( e3%b%a ) /= 0)                ERROR STOP 62_4
    IF (SIZE( e3%base%a ) /= 5)             ERROR STOP 63_4


    CONTAINS

        INTEGER FUNCTION NewExt( extArg )
            TYPE(ext(*,8,*)), ALLOCATABLE :: extArg

            CHARACTER(255) :: errmsg

            ALLOCATE(extArg, STAT=NewExt, ERRMSG=errmsg)
            IF (NewExt /= 0) THEN
                PRINT *, 'ALLOCATE(extArg,STAT=', NewExt, ') ', errmsg
            END IF

        END FUNCTION NewExt

END PROGRAM allocExtVariable05
