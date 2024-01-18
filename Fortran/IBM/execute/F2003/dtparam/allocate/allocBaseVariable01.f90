!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : allocBaseVariable01
!*  TEST CASE TITLE            : ALLOCATE() Statement with DTP
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : December  4, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is a Base Derived Type
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


    IF ( ALLOCATED( b1 ) )      STOP 10

    ALLOCATE(b1, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, 'ALLOCATE(b1,STAT=', stat, ') ', errmsg
        STOP 11
    END IF

    IF (.NOT. ALLOCATED( b1 ))              STOP 12

    IF (b1%k /= 4)                          STOP 13
    IF (KIND( b1%array ) /= 4)              STOP 14

    IF ( ANY(SHAPE( b1%array ) /= [ 6 ]) )  STOP 15

    IF (b1%l1 /= 5)                         STOP 16
    IF (LBOUND(b1%array, 1) /= 5)           STOP 17

    IF (b1%l2 /= 10)                        STOP 18
    IF (UBOUND(b1%array, 1) /= 10)          STOP 19


    IF ( ALLOCATED( b2 ) )      STOP 20

    ALLOCATE(b2, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, 'ALLOCATE(b2,STAT=', stat, ') ', errmsg
        STOP 21
    END IF

    IF (.NOT. ALLOCATED( b2 ))              STOP 22

    IF (b2%k /= 8)                          STOP 23
    IF (KIND( b2%array ) /= 8)              STOP 24

    PRINT *, SHAPE( b2%array ), SIZE( b2%array )
    IF ( ANY(SHAPE( b2%array ) /= [ 1 ]) )  STOP 25

    IF (b2%l1 /= 10)                        STOP 26
    IF (LBOUND(b2%array, 1) /= 10)          STOP 27

    IF (b2%l2 /= 10)                         STOP 28
    IF (UBOUND(b2%array, 1) /= 10)           STOP 29


    IF ( ALLOCATED( b3 ) )      STOP 30

    ALLOCATE(b3, STAT=stat, ERRMSG=errmsg)
    IF (stat /= 0) THEN
        PRINT *, 'ALLOCATE(b3,STAT=', stat, ') ', errmsg
        STOP 31
    END IF

    IF (.NOT. ALLOCATED( b3 ))              STOP 32

    IF (b3%k /= 2)                          STOP 33
    IF (KIND( b3%array ) /= 2)              STOP 34

    IF ( ANY(SHAPE( b3%array ) /= [ 2 ]) )  STOP 35

    IF (b3%l1 /= -1)                        STOP 36
    IF (LBOUND(b3%array, 1) /= -1)          STOP 37

    IF (b3%l2 /= 0)                         STOP 38
    IF (UBOUND(b3%array, 1) /= 0)           STOP 39


END PROGRAM allocBaseVariable01
