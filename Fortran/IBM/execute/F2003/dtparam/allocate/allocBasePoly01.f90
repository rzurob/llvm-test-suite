!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : allocBasePoly01
!*
!*  DATE                       : January  5, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is a Polymorphic of a Base Derived Type
!*  SECONDARY FUNCTIONS TESTED : and the allocation-list contains variable
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Where allocation-list (without type-spec:: and using:  Base Derived
!*  Types) is:
!*  o  Polymorphic Derived Types
!*
!*  Types of allocation-list Objects:
!*  o  Variable
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE baseMod

    IMPLICIT NONE

    TYPE base(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        CHARACTER(l1) :: typeOfStuff
        INTEGER(k1), POINTER :: stuff( : )
    END TYPE base

END MODULE baseMod


PROGRAM allocBasePoly01
    USE baseMod

    IMPLICIT NONE

    INTEGER :: stat
    CHARACTER(255) :: errmsg

    CLASS(base(8,5)), POINTER :: basePtr
    CLASS(base(8,5)), ALLOCATABLE, TARGET :: baseAlloc


    IF ( ASSOCIATED( basePtr ) )                STOP 10


    ALLOCATE(basePtr, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, "ALLOCATE(basePtr,STAT=", stat, ") ", errmsg
        STOP 20
    END IF


    IF (.NOT. ASSOCIATED( basePtr ))            STOP 21

    IF (basePtr%k1 /= 8)                        STOP 22
    IF (KIND( basePtr%stuff ) /= 8)             STOP 23

    IF (basePtr%l1 /= 5)                        STOP 24
    IF (LEN( basePtr%typeOfStuff ) /= 5)        STOP 25


    DEALLOCATE( basePtr )

    IF ( ASSOCIATED( basePtr ) )                STOP 30



    basePtr => baseAlloc

    IF ( ALLOCATED( baseAlloc ) )               STOP 40
    IF (.NOT. ASSOCIATED( basePtr ))            STOP 41


    ALLOCATE(baseAlloc, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, "ALLOCATE(baseAlloc,STAT=", stat, ") ", errmsg
        STOP 50
    END IF

    IF (.NOT. ALLOCATED( baseAlloc ))           STOP 51

    IF (baseAlloc%k1 /= 8)                      STOP 52
    IF (KIND( baseAlloc%stuff ) /= 8)           STOP 53

    IF (baseAlloc%l1 /= 5)                      STOP 54
    IF (LEN( baseAlloc%typeOfStuff ) /= 5)      STOP 55


    IF (.NOT. ASSOCIATED( basePtr ))            STOP 61

    IF (basePtr%k1 /= 8)                        STOP 62
    IF (KIND( basePtr%stuff ) /= 8)             STOP 63

    IF (basePtr%l1 /= 5)                        STOP 64
    IF (LEN( basePtr%typeOfStuff ) /= 5)        STOP 65


    DEALLOCATE( baseAlloc )

    IF ( ALLOCATED( baseAlloc ) )               STOP 70
    IF (.NOT. ASSOCIATED( basePtr ))            STOP 71


    ALLOCATE(basePtr, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, "ALLOCATE(basePtr,STAT=", stat, ") ", errmsg
        STOP 80
    END IF

    IF ( ALLOCATED( baseAlloc ) )               STOP 81


    IF (.NOT. ASSOCIATED( basePtr ))            STOP 91

    IF (basePtr%k1 /= 8)                        STOP 92
    IF (KIND( basePtr%stuff ) /= 8)             STOP 93

    IF (basePtr%l1 /= 5)                        STOP 94
    IF (LEN( basePtr%typeOfStuff ) /= 5)        STOP 95


    DEALLOCATE( basePtr )

    IF ( ALLOCATED( baseAlloc ) )               STOP 100
    IF ( ASSOCIATED( basePtr ) )                STOP 101

END PROGRAM allocBasePoly01
