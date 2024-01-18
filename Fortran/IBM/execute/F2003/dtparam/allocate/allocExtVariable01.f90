!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : December  5, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is an Extended Derived Type
!*  SECONDARY FUNCTIONS TESTED : and the allocation-list contains a variable
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
!*  o  Variable
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


PROGRAM allocExtVariable01
    USE extMod

    IMPLICIT NONE

    INTEGER :: stat
    CHARACTER(255) :: errmsg

    TYPE(ext(5,16,1)), ALLOCATABLE  :: e1
    TYPE(ext(0,4,1)), ALLOCATABLE   :: e2
    TYPE(ext(5,8,0)), ALLOCATABLE   :: e3


    IF ( ALLOCATED( e1 ) )                  ERROR STOP 10

    ALLOCATE(e1, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, 'ALLOCATE(e1,STAT=', stat, ') ', errmsg
        STOP 11
    END IF

    IF (.NOT. ALLOCATED( e1 ))              ERROR STOP 12

    IF (e1%k /= 16)                         ERROR STOP 13
    IF (e1%b%k /= 16)                       ERROR STOP 14
    IF (e1%base%k /= 16)                    ERROR STOP 15
    IF (KIND( e1%b%a ) /= 16)               ERROR STOP 16
    IF (KIND( e1%base%a ) /= 16)            ERROR STOP 17

    IF (e1%l2 /= 1)                         ERROR STOP 18
    IF (e1%l1 /= 5)                         ERROR STOP 19
    IF (e1%b%l1 /= 5)                       ERROR STOP 20
    IF (e1%base%l1 /= 5)                    ERROR STOP 21

    IF (SIZE( e1%b%a ) /= 5)                ERROR STOP 22
    IF (SIZE( e1%base%a ) /= 5)             ERROR STOP 23


    IF ( ALLOCATED( e2 ) )                  ERROR STOP 30

    ALLOCATE(e2, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, 'ALLOCATE(e2,STAT=', stat, ') ', errmsg
        STOP 31
    END IF

    IF (.NOT. ALLOCATED( e2 ))              ERROR STOP 32

    IF (e2%k /= 4)                          ERROR STOP 33
    IF (e2%b%k /= 4)                        ERROR STOP 34
    IF (e2%base%k /= 4)                     ERROR STOP 35
    IF (KIND( e2%b%a ) /= 4)                ERROR STOP 36
    IF (KIND( e2%base%a ) /= 4)             ERROR STOP 37

    IF (e2%l2 /= 1)                         ERROR STOP 38
    IF (e2%l1 /= 0)                         ERROR STOP 39
    IF (e2%b%l1 /= 0)                       ERROR STOP 40
    IF (e2%base%l1 /= 0)                    ERROR STOP 41

    IF (SIZE( e2%b%a ) /= 0)                ERROR STOP 42
    IF (SIZE( e2%base%a ) /= 0)             ERROR STOP 43


    IF ( ALLOCATED( e3 ) )                  ERROR STOP 50

    ALLOCATE(e3, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, 'ALLOCATE(e3,STAT=', stat, ') ', errmsg
        STOP 51
    END IF

    IF (.NOT. ALLOCATED( e3 ))              ERROR STOP 52

    IF (e3%k /= 8)                          ERROR STOP 53
    IF (e3%b%k /= 8)                        ERROR STOP 54
    IF (e3%base%k /= 8)                     ERROR STOP 55
    IF (KIND( e3%b%a ) /= 8)                ERROR STOP 56
    IF (KIND( e3%base%a ) /= 8)             ERROR STOP 57

    IF (e3%l2 /= 0)                         ERROR STOP 58
    IF (e3%l1 /= 5)                         ERROR STOP 59
    IF (e3%b%l1 /= 0)                       ERROR STOP 60
    IF (e3%base%l1 /= 5)                    ERROR STOP 61

    IF (SIZE( e3%b%a ) /= 0)                ERROR STOP 62
    IF (SIZE( e3%base%a ) /= 5)             ERROR STOP 63

END PROGRAM allocExtVariable01
