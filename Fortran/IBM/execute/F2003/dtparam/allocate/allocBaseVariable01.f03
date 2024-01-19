!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : December  4, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is a Base Derived Type
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
!*  o  Basic Derived Types
!*
!*  Types of allocation-list Objects:
!*  o  Variable
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE typeMod

    IMPLICIT NONE

    TYPE base(k,l1,l2)
        INTEGER, LEN :: l1
        INTEGER, LEN :: l2
        INTEGER, KIND :: k

        LOGICAL(k) :: array( l1:l2 )
    END TYPE base

END MODULE typeMod


PROGRAM allocBaseVariable01
    USE typeMod

    IMPLICIT NONE

    INTEGER :: stat
    CHARACTER(255) :: errmsg

    TYPE(base(4,5,10)), ALLOCATABLE :: b1
    TYPE(base(8,10,10)), ALLOCATABLE :: b2
    TYPE(base(2,-1,0)), ALLOCATABLE :: b3


    IF ( ALLOCATED( b1 ) )      ERROR STOP 10

    ALLOCATE(b1, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, 'ALLOCATE(b1,STAT=', stat, ') ', errmsg
        STOP 11
    END IF

    IF (.NOT. ALLOCATED( b1 ))              ERROR STOP 12

    IF (b1%k /= 4)                          ERROR STOP 13
    IF (KIND( b1%array ) /= 4)              ERROR STOP 14

    IF ( ANY(SHAPE( b1%array ) /= [ 6 ]) )  ERROR STOP 15

    IF (b1%l1 /= 5)                         ERROR STOP 16
    IF (LBOUND(b1%array, 1) /= 5)           ERROR STOP 17

    IF (b1%l2 /= 10)                        ERROR STOP 18
    IF (UBOUND(b1%array, 1) /= 10)          ERROR STOP 19


    IF ( ALLOCATED( b2 ) )      ERROR STOP 20

    ALLOCATE(b2, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, 'ALLOCATE(b2,STAT=', stat, ') ', errmsg
        STOP 21
    END IF

    IF (.NOT. ALLOCATED( b2 ))              ERROR STOP 22

    IF (b2%k /= 8)                          ERROR STOP 23
    IF (KIND( b2%array ) /= 8)              ERROR STOP 24

    PRINT *, SHAPE( b2%array ), SIZE( b2%array )
    IF ( ANY(SHAPE( b2%array ) /= [ 1 ]) )  ERROR STOP 25

    IF (b2%l1 /= 10)                        ERROR STOP 26
    IF (LBOUND(b2%array, 1) /= 10)          ERROR STOP 27

    IF (b2%l2 /= 10)                         ERROR STOP 28
    IF (UBOUND(b2%array, 1) /= 10)           ERROR STOP 29


    IF ( ALLOCATED( b3 ) )      ERROR STOP 30

    ALLOCATE(b3, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, 'ALLOCATE(b3,STAT=', stat, ') ', errmsg
        STOP 31
    END IF

    IF (.NOT. ALLOCATED( b3 ))              ERROR STOP 32

    IF (b3%k /= 2)                          ERROR STOP 33
    IF (KIND( b3%array ) /= 2)              ERROR STOP 34

    IF ( ANY(SHAPE( b3%array ) /= [ 2 ]) )  ERROR STOP 35

    IF (b3%l1 /= -1)                        ERROR STOP 36
    IF (LBOUND(b3%array, 1) /= -1)          ERROR STOP 37

    IF (b3%l2 /= 0)                         ERROR STOP 38
    IF (UBOUND(b3%array, 1) /= 0)           ERROR STOP 39


END PROGRAM allocBaseVariable01
