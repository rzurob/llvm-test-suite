!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : January 13, 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : The ALLOCATE Statement has an allocation-list
!*                               that is a Base Derived Type
!*  SECONDARY FUNCTIONS TESTED : and the allocation-list contains Function
!*                               Return Value
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
!*  o  Function Return values
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE typeMod

    IMPLICIT NONE

    TYPE base(k,l1,l2)
        INTEGER, LEN :: l1
        INTEGER, LEN :: l2
        INTEGER, KIND :: k

        LOGICAL(k) :: array( l1:l2 )

        CONTAINS

            PROCEDURE, PASS :: NewBase2
            PROCEDURE, PASS :: NewBase4
            PROCEDURE, PASS :: NewBase8

            GENERIC :: NewBase => NewBase2, NewBase4, NewBase8

    END TYPE base

    CONTAINS

        FUNCTION NewBase2(this, rc)
            CLASS(base(2,*,*)) :: this
            INTEGER(4) :: rc

            TYPE(base(2,-1,0)), POINTER :: NewBase2

            INTEGER :: stat
            CHARACTER(255) :: errmsg

            ALLOCATE(NewBase2, STAT=stat, ERRMSG=errmsg)
            IF (stat /= 0) THEN
                PRINT *, 'ALLOCATE(NewBase2,STAT=', stat, ') ', errmsg
                CALL zzrc( rc )
            END IF

        END FUNCTION NewBase2

        FUNCTION NewBase4(this, rc)
            CLASS(base(4,*,*)) :: this
            INTEGER(4) :: rc

            TYPE(base(4,5,10)), POINTER :: NewBase4

            INTEGER :: stat
            CHARACTER(255) :: errmsg

            ALLOCATE(NewBase4, STAT=stat, ERRMSG=errmsg)
            IF (stat /= 0) THEN
                PRINT *, 'ALLOCATE(NewBase4,STAT=', stat, ') ', errmsg
                CALL zzrc( rc )
            END IF

        END FUNCTION NewBase4

        FUNCTION NewBase8(this, rc)
            CLASS(base(8,*,*)) :: this
            INTEGER(4) :: rc

            TYPE(base(8,10,10)), POINTER :: NewBase8

            INTEGER :: stat
            CHARACTER(255) :: errmsg

            ALLOCATE(NewBase8, STAT=stat, ERRMSG=errmsg)
            IF (stat /= 0) THEN
                PRINT *, 'ALLOCATE(NewBase8,STAT=', stat, ') ', errmsg
                CALL zzrc( rc )
            END IF

        END FUNCTION NewBase8

END MODULE typeMod


PROGRAM allocBaseVariable03
    USE typeMod

    IMPLICIT NONE

    TYPE(base(4,5,10)), POINTER :: b1
    TYPE(base(8,10,10)), POINTER :: b2
    TYPE(base(2,-1,0)), POINTER :: b3


    IF ( ASSOCIATED( b1 ) )                 ERROR STOP 10_4

    b1 => b1%NewBase( 11_4 )

    IF (.NOT. ASSOCIATED( b1 ))             ERROR STOP 12_4

    IF (b1%k /= 4)                          ERROR STOP 13_4
    IF (KIND( b1%array ) /= 4)              ERROR STOP 14_4

    IF ( ANY(SHAPE( b1%array ) /= [ 6 ]) )  ERROR STOP 15_4

    IF (b1%l1 /= 5)                         ERROR STOP 16_4
    IF (LBOUND(b1%array, 1) /= 5)           ERROR STOP 17_4

    IF (b1%l2 /= 10)                        ERROR STOP 18_4
    IF (UBOUND(b1%array, 1) /= 10)          ERROR STOP 19_4


    IF ( ASSOCIATED( b2 ) )                 ERROR STOP 20_4

    b2 => b2%NewBase( 21_4 )

    IF (.NOT. ASSOCIATED( b2 ))             ERROR STOP 22_4

    IF (b2%k /= 8)                          ERROR STOP 23_4
    IF (KIND( b2%array ) /= 8)              ERROR STOP 24_4

    IF ( ANY(SHAPE( b2%array ) /= [ 1 ]) )  ERROR STOP 25_4

    IF (b2%l1 /= 10)                        ERROR STOP 26_4
    IF (LBOUND(b2%array, 1) /= 10)          ERROR STOP 27_4

    IF (b2%l2 /= 10)                         ERROR STOP 28_4
    IF (UBOUND(b2%array, 1) /= 10)           ERROR STOP 29_4


    IF ( ASSOCIATED( b3 ) )                 ERROR STOP 30_4

    b3 => b3%NewBase( 41_4 )

    IF (.NOT. ASSOCIATED( b3 ))             ERROR STOP 32_4

    IF (b3%k /= 2)                          ERROR STOP 33_4
    IF (KIND( b3%array ) /= 2)              ERROR STOP 34_4

    IF ( ANY(SHAPE( b3%array ) /= [ 2 ]) )  ERROR STOP 35_4

    IF (b3%l1 /= -1)                        ERROR STOP 36_4
    IF (LBOUND(b3%array, 1) /= -1)          ERROR STOP 37_4

    IF (b3%l2 /= 0)                         ERROR STOP 38_4
    IF (UBOUND(b3%array, 1) /= 0)           ERROR STOP 39_4


END PROGRAM allocBaseVariable03
