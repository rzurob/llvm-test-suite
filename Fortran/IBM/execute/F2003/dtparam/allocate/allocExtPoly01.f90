!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : allocExtPoly01
!*  TEST CASE TITLE            : ALLOCATE() Statement with DTP
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January  5, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is a Polymorphic of an Extended Derived
!*                               Type
!*  SECONDARY FUNCTIONS TESTED : and the allocation-list contains variable
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Where allocation-list (without type-spec:: and using:  Extended Derived
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


MODULE extMod
    USE baseMod

    IMPLICIT NONE

    TYPE, EXTENDS(base) :: ext(l2)
        INTEGER, LEN :: l2

        INTEGER(k1) :: moreStuff( l1,l2:l1 )
    END TYPE ext

END MODULE extMod


PROGRAM allocExtPoly01
    USE extMod

    IMPLICIT NONE

    INTEGER :: stat
    CHARACTER(255) :: errmsg

    CLASS(ext(8,5,6)), POINTER :: extPtr
    CLASS(ext(8,5,6)), ALLOCATABLE, TARGET :: extAlloc


    IF ( ASSOCIATED( extPtr ) )                         STOP 10


    ALLOCATE(extPtr, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, "ALLOCATE(extPtr,STAT=", stat, ") ", errmsg
        STOP 20
    END IF


    IF (.NOT. ASSOCIATED( extPtr ))                     STOP 21

    IF (extPtr%k1 /= 8)                                 STOP 22
    IF (KIND( extPtr%stuff ) /= 8)                      STOP 23

    IF (extPtr%l1 /= 5)                                 STOP 24
    IF (extPtr%l2 /= 6)                                 STOP 25

    IF (LEN( extPtr%typeOfStuff ) /= 5)                 STOP 26
    IF ( ANY(SHAPE( extPtr%moreStuff ) /= [ 5,0 ]) )    STOP 27


    DEALLOCATE( extPtr )

    IF ( ASSOCIATED( extPtr ) )                         STOP 30



    extPtr => extAlloc

    IF ( ALLOCATED( extAlloc ) )                        STOP 40
    IF (.NOT. ASSOCIATED( extPtr ))                     STOP 41


    ALLOCATE(extAlloc, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, "ALLOCATE(extAlloc,STAT=", stat, ") ", errmsg
        STOP 50
    END IF

    IF (.NOT. ALLOCATED( extAlloc ))                    STOP 51

    IF (extAlloc%k1 /= 8)                               STOP 52
    IF (KIND( extAlloc%stuff ) /= 8)                    STOP 53

    IF (extAlloc%l1 /= 5)                               STOP 54
    IF (extAlloc%l2 /= 6)                               STOP 55

    IF (LEN( extAlloc%typeOfStuff ) /= 5)               STOP 56
    IF ( ANY(SHAPE( extAlloc%moreStuff ) /= [ 5,0 ]) )  STOP 57


    IF (.NOT. ASSOCIATED( extPtr ))                     STOP 61

    IF (extPtr%k1 /= 8)                                 STOP 62
    IF (KIND( extPtr%stuff ) /= 8)                      STOP 63

    IF (extPtr%l1 /= 5)                                 STOP 64
    IF (extPtr%l2 /= 6)                                 STOP 65

    IF (LEN( extPtr%typeOfStuff ) /= 5)                 STOP 66
    IF ( ANY(SHAPE( extPtr%moreStuff ) /= [ 5,0 ]) )    STOP 67


    DEALLOCATE( extAlloc )

    IF ( ALLOCATED( extAlloc ) )                        STOP 70
    IF (.NOT. ASSOCIATED( extPtr ))                     STOP 71


    ALLOCATE(extPtr, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, "ALLOCATE(extPtr,STAT=", stat, ") ", errmsg
        STOP 80
    END IF

    IF ( ALLOCATED( extAlloc ) )                        STOP 81


    IF (.NOT. ASSOCIATED( extPtr ))                     STOP 91

    IF (extPtr%k1 /= 8)                                 STOP 92
    IF (KIND( extPtr%stuff ) /= 8)                      STOP 93

    IF (extPtr%l1 /= 5)                                 STOP 94
    IF (extPtr%l2 /= 6)                                 STOP 95

    IF (LEN( extPtr%typeOfStuff ) /= 5)                 STOP 96
    IF ( ANY(SHAPE( extPtr%moreStuff ) /= [ 5,0 ]) )    STOP 97


    DEALLOCATE( extPtr )

    IF ( ALLOCATED( extAlloc ) )                        STOP 100
    IF ( ASSOCIATED( extPtr ) )                         STOP 101

END PROGRAM allocExtPoly01
