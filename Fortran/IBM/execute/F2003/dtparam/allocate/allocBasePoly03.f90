!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : allocBasePoly03
!*  TEST CASE TITLE            : ALLOCATE() Statement with DTP
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January 12, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is a Polymorphic of a Base Derived Type
!*  SECONDARY FUNCTIONS TESTED : and the allocation-list contains a Function
!*                               Return Value
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
!*  o  Polymorphic Derived Types
!*
!*  Types of allocation-list Objects:
!*  o  Function Return values
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

    CONTAINS

        FUNCTION NewBasePtr( rc )
            INTEGER(4) :: rc

            CLASS(base(8,5)), POINTER :: NewBasePtr

            INTEGER :: stat
            CHARACTER(255) :: errmsg

            ALLOCATE(NewBasePtr, STAT=stat, ERRMSG=errmsg)
            IF (stat /= 0) THEN
                PRINT *, "ALLOCATE(NewBasePtr(8,5),STAT=", stat, ") ", errmsg
                CALL zzrc( rc )
            END IF

        END FUNCTION NewBasePtr

END MODULE baseMod


PROGRAM allocBasePoly03
    USE baseMod

    IMPLICIT NONE

    CLASS(base(8,5)), POINTER :: basePtr


    IF ( ASSOCIATED( basePtr ) )                CALL zzrc( 10_4 )


    basePtr => NewBasePtr( 20_4 )


    IF (.NOT. ASSOCIATED( basePtr ))            CALL zzrc( 21_4 )

    IF (basePtr%k1 /= 8)                        CALL zzrc( 22_4 )
    IF (KIND( basePtr%stuff ) /= 8)             CALL zzrc( 23_4 )

    IF (basePtr%l1 /= 5)                        CALL zzrc( 24_4 )
    IF (LEN( basePtr%typeOfStuff ) /= 5)        CALL zzrc( 25_4 )


    DEALLOCATE( basePtr )

    IF ( ASSOCIATED( basePtr ) )                CALL zzrc( 30_4 )

END PROGRAM allocBasePoly03
