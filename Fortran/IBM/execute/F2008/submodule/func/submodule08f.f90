!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : December 20, 2012
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : F2008 submodule
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  based on F2003/dtparam/allocate/allocBaseVariable03.f
!*
!*  Generic function pointer resolution to functions declared in an
!*   interface and defined in a different descendant submodules among
!*   the submodules represented in this tree structure:
!*                       m
!*                      / \
!*                     m1 m2
!*                        / \
!*                       m3 m4
!*
!* Secondary tests:
!* - submodule m2 does not define anything (blank)
!*
!* Verify that the results match the values of the original test case.
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

    INTERFACE

        MODULE FUNCTION NewBase2(this, rc)
            CLASS(base(2,*,*)) :: this
            TYPE(base(2,-1,0)), POINTER :: NewBase2
            INTEGER(4) :: rc
        END FUNCTION

        MODULE FUNCTION NewBase4(this, rc)
            CLASS(base(4,*,*)) :: this
            TYPE(base(4,5,10)), POINTER :: NewBase4
            INTEGER(4) :: rc
        END FUNCTION

        MODULE FUNCTION NewBase8(this, rc)
            CLASS(base(8,*,*)) :: this
            TYPE(base(8,10,10)), POINTER :: NewBase8
            INTEGER(4) :: rc
        END FUNCTION

    END INTERFACE

END MODULE typeMod

SUBMODULE (typeMod) subMod

    CONTAINS

        MODULE FUNCTION NewBase2(this, rc)
            CLASS(base(2,*,*)) :: this
            INTEGER(4) :: rc

            TYPE(base(2,-1,0)), POINTER :: NewBase2

            INTEGER :: stat
            CHARACTER(255) :: errmsg

            ALLOCATE(NewBase2, STAT=stat, ERRMSG=errmsg)
            IF (stat /= 0) THEN
                PRINT *, 'ALLOCATE(NewBase2,STAT=', stat, ') ', errmsg
                call zzrc( rc )
            END IF

        END FUNCTION NewBase2

END SUBMODULE subMod

SUBMODULE (typeMod) node2
END SUBMODULE node2

SUBMODULE (typeMod:node2) node3

CONTAINS

        MODULE FUNCTION NewBase4(this, rc)
            CLASS(base(4,*,*)) :: this
            INTEGER(4) :: rc

            TYPE(base(4,5,10)), POINTER :: NewBase4

            INTEGER :: stat
            CHARACTER(255) :: errmsg

            ALLOCATE(NewBase4, STAT=stat, ERRMSG=errmsg)
            IF (stat /= 0) THEN
                PRINT *, 'ALLOCATE(NewBase4,STAT=', stat, ') ', errmsg
                call zzrc( rc )
            END IF

        END FUNCTION NewBase4

END SUBMODULE node3

SUBMODULE (typeMod:node2) node4

CONTAINS

        MODULE FUNCTION NewBase8(this, rc)
            CLASS(base(8,*,*)) :: this
            INTEGER(4) :: rc

            TYPE(base(8,10,10)), POINTER :: NewBase8

            INTEGER :: stat
            CHARACTER(255) :: errmsg

            ALLOCATE(NewBase8, STAT=stat, ERRMSG=errmsg)
            IF (stat /= 0) THEN
                PRINT *, 'ALLOCATE(NewBase8,STAT=', stat, ') ', errmsg
                call zzrc( rc )
            END IF

        END FUNCTION NewBase8

END SUBMODULE node4


PROGRAM allocBaseVariable03
    USE typeMod

    IMPLICIT NONE

    TYPE(base(4,5,10)), POINTER :: b1
    TYPE(base(8,10,10)), POINTER :: b2
    TYPE(base(2,-1,0)), POINTER :: b3


    IF ( ASSOCIATED( b1 ) )                 error stop  10_4

    b1 => b1%NewBase( 11_4 )

    IF (.NOT. ASSOCIATED( b1 ))             error stop  12_4

    IF (b1%k /= 4)                          error stop  13_4
    IF (KIND( b1%array ) /= 4)              error stop  14_4

    IF ( ANY(SHAPE( b1%array ) /= [ 6 ]) )  error stop  15_4

    IF (b1%l1 /= 5)                         error stop  16_4
    IF (LBOUND(b1%array, 1) /= 5)           error stop  17_4

    IF (b1%l2 /= 10)                        error stop  18_4
    IF (UBOUND(b1%array, 1) /= 10)          error stop  19_4


    IF ( ASSOCIATED( b2 ) )                 error stop  20_4

    b2 => b2%NewBase( 21_4 )

    IF (.NOT. ASSOCIATED( b2 ))             error stop  22_4

    IF (b2%k /= 8)                          error stop  23_4
    IF (KIND( b2%array ) /= 8)              error stop  24_4

    IF ( ANY(SHAPE( b2%array ) /= [ 1 ]) )  error stop  25_4

    IF (b2%l1 /= 10)                        error stop  26_4
    IF (LBOUND(b2%array, 1) /= 10)          error stop  27_4

    IF (b2%l2 /= 10)                         error stop  28_4
    IF (UBOUND(b2%array, 1) /= 10)           error stop  29_4


    IF ( ASSOCIATED( b3 ) )                 error stop  30_4

    b3 => b3%NewBase( 41_4 )

    IF (.NOT. ASSOCIATED( b3 ))             error stop  32_4

    IF (b3%k /= 2)                          error stop  33_4
    IF (KIND( b3%array ) /= 2)              error stop  34_4

    IF ( ANY(SHAPE( b3%array ) /= [ 2 ]) )  error stop  35_4

    IF (b3%l1 /= -1)                        error stop  36_4
    IF (LBOUND(b3%array, 1) /= -1)          error stop  37_4

    IF (b3%l2 /= 0)                         error stop  38_4
    IF (UBOUND(b3%array, 1) /= 0)           error stop  39_4


END PROGRAM allocBaseVariable03
