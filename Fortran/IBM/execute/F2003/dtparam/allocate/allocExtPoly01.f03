!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : January  5, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is a Polymorphic of an Extended Derived
!*                               Type
!*  SECONDARY FUNCTIONS TESTED : and the allocation-list contains variable
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


    IF ( ASSOCIATED( extPtr ) )                         ERROR STOP 10


    ALLOCATE(extPtr, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, "ALLOCATE(extPtr,STAT=", stat, ") ", errmsg
        STOP 20
    END IF


    IF (.NOT. ASSOCIATED( extPtr ))                     ERROR STOP 21

    IF (extPtr%k1 /= 8)                                 ERROR STOP 22
    IF (KIND( extPtr%stuff ) /= 8)                      ERROR STOP 23

    IF (extPtr%l1 /= 5)                                 ERROR STOP 24
    IF (extPtr%l2 /= 6)                                 ERROR STOP 25

    IF (LEN( extPtr%typeOfStuff ) /= 5)                 ERROR STOP 26
    IF ( ANY(SHAPE( extPtr%moreStuff ) /= [ 5,0 ]) )    ERROR STOP 27


    DEALLOCATE( extPtr )

    IF ( ASSOCIATED( extPtr ) )                         ERROR STOP 30



    extPtr => extAlloc

    IF ( ALLOCATED( extAlloc ) )                        ERROR STOP 40
    IF (.NOT. ASSOCIATED( extPtr ))                     ERROR STOP 41


    ALLOCATE(extAlloc, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, "ALLOCATE(extAlloc,STAT=", stat, ") ", errmsg
        STOP 50
    END IF

    IF (.NOT. ALLOCATED( extAlloc ))                    ERROR STOP 51

    IF (extAlloc%k1 /= 8)                               ERROR STOP 52
    IF (KIND( extAlloc%stuff ) /= 8)                    ERROR STOP 53

    IF (extAlloc%l1 /= 5)                               ERROR STOP 54
    IF (extAlloc%l2 /= 6)                               ERROR STOP 55

    IF (LEN( extAlloc%typeOfStuff ) /= 5)               ERROR STOP 56
    IF ( ANY(SHAPE( extAlloc%moreStuff ) /= [ 5,0 ]) )  ERROR STOP 57


    IF (.NOT. ASSOCIATED( extPtr ))                     ERROR STOP 61

    IF (extPtr%k1 /= 8)                                 ERROR STOP 62
    IF (KIND( extPtr%stuff ) /= 8)                      ERROR STOP 63

    IF (extPtr%l1 /= 5)                                 ERROR STOP 64
    IF (extPtr%l2 /= 6)                                 ERROR STOP 65

    IF (LEN( extPtr%typeOfStuff ) /= 5)                 ERROR STOP 66
    IF ( ANY(SHAPE( extPtr%moreStuff ) /= [ 5,0 ]) )    ERROR STOP 67


    DEALLOCATE( extAlloc )

    IF ( ALLOCATED( extAlloc ) )                        ERROR STOP 70
    IF (.NOT. ASSOCIATED( extPtr ))                     ERROR STOP 71


    ALLOCATE(extPtr, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, "ALLOCATE(extPtr,STAT=", stat, ") ", errmsg
        STOP 80
    END IF

    IF ( ALLOCATED( extAlloc ) )                        ERROR STOP 81


    IF (.NOT. ASSOCIATED( extPtr ))                     ERROR STOP 91

    IF (extPtr%k1 /= 8)                                 ERROR STOP 92
    IF (KIND( extPtr%stuff ) /= 8)                      ERROR STOP 93

    IF (extPtr%l1 /= 5)                                 ERROR STOP 94
    IF (extPtr%l2 /= 6)                                 ERROR STOP 95

    IF (LEN( extPtr%typeOfStuff ) /= 5)                 ERROR STOP 96
    IF ( ANY(SHAPE( extPtr%moreStuff ) /= [ 5,0 ]) )    ERROR STOP 97


    DEALLOCATE( extPtr )

    IF ( ALLOCATED( extAlloc ) )                        ERROR STOP 100
    IF ( ASSOCIATED( extPtr ) )                         ERROR STOP 101

END PROGRAM allocExtPoly01