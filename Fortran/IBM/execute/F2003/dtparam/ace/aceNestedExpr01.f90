!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : aceNestedExpr01
!*  TEST CASE TITLE            : Basic (non-Polymorphic) Array Constructors
!*                               with DTP
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : November 12, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor contains Nested Array
!*                               Constructors for a Derived Type (with
!*                               Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : and is the expr of an Intrinsic Assignment
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  1)  Testing the format of the Array Constructor:
!*      * Brackets:  []
!*      * Where the ac-value-list contains:
!*        o Nested Array Constructors with:
!*          - Scalar Variables,
!*          - Structure Constructors, and
!*          - Scalar Variables (with Deferred Length Parameters).
!*
!*  2)  Testing the usage of an Array Constructor in various contexts:
!*      * As the expr of an Intrinsic Assignment, and
!*      * As the value of a SOURCE= Specifier in an ALLOCATE Statement.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE bMod

    IMPLICIT NONE

    TYPE base(k1,l1,l2)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1
        INTEGER, LEN :: l2

        CHARACTER(l1) :: lable
        REAL(k1) :: array( l2 )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual8
            PROCEDURE, PASS :: NotEqual8 => Base8NotEqual

    END TYPE base

    TYPE(base(8,:,:)), POINTER :: i1 => NULL()
    TYPE(base(8,:,:)), POINTER :: i2 => NULL()
    TYPE(base(8,:,:)), POINTER :: i3 => NULL()

    CONTAINS

        LOGICAL FUNCTION Base8NotEqual(this, o)
            CLASS(base(8,*,*)), INTENT(in) :: this
            CLASS(base(8,*,*)), INTENT(in) :: o


            Base8NotEqual = .TRUE.
            IF ((this%k1 == o%k1) .AND.&
                (this%l1 == o%l1) .AND.&
                (this%l2 == o%l2) .AND.&
                (this%lable == o%lable) .AND.&
                ( ALL(this%array == o%array) )) THEN
                Base8NotEqual = .FALSE.
            END IF

        END FUNCTION Base8NotEqual

END MODULE bMod


PROGRAM aceNestedExpr01
    USE bMod

    IMPLICIT NONE

    INTERFACE
        SUBROUTINE BaseElementsAsArguments(r, rc)
            USE bMod

            IMPLICIT NONE

            REAL(8) :: r( * )
            INTEGER(4) :: rc
        END SUBROUTINE BaseElementsAsArguments
    END INTERFACE


    INTEGER :: i
    INTEGER :: j
    INTEGER :: stat

    CHARACTER(255) :: errmsg

    REAL(8) :: rA( 9 ) = [ ((1.0_8 / REAL(i, 8)), i = 1, 9) ]


    TYPE(base(8,3,3)), TARGET :: item1
    TYPE(base(8,3,3)), TARGET :: item2
    TYPE(base(8,3,3)), TARGET :: item3

    TYPE(base(8,:,:)), POINTER :: baseA( : )
    TYPE(base(8,3,3)), POINTER :: itemPtr


    item1 = base(8,3,3)('IBM',rA( 1:3 ))
    item2 = base(8,3,3)('XLF',rA( 4:6 ))
    item3 = base(8,3,3)('TST',rA( 7:9 ))


    ALLOCATE(baseA( 9 ), STAT=stat, ERRMSG=errmsg,&
                SOURCE=[    [ item1, item2, item3 ],&
                            [   base(8,3,3)('IBM',rA( 1:3 )),&
                                base(8,3,3)('XLF',rA( 4:6 )),&
                                base(8,3,3)('TST',rA( 7:9 ))    ],&
                            [ item1, item2, item3 ] ])

    IF (stat /= 0) THEN
        PRINT *, 'ALLOCATE(', stat, ') ', errmsg
        CALL zzrc( 10_4 )
    END IF


    IF (SIZE( baseA ) /= 9)         CALL zzrc( 20_4 )

    DO i = 1, SIZE( baseA )
        j = MOD((i - 1), 3) + 1

        itemPtr => item1
        IF (j == 2) THEN
            itemPtr => item2
        ELSE IF (j > 2) THEN
            itemPtr => item3
        END IF

        IF (baseA( i ) /= itemPtr)    CALL zzrc( (20_4 + INT(i, 4)) )
    END DO


    i1 => item1
    i2 => item2
    i3 => item3

    CALL BaseElementsAsArguments(rA, 30_4)

END PROGRAM aceNestedExpr01


SUBROUTINE BaseElementsAsArguments(r, rc)
    USE bMod

    IMPLICIT NONE

    REAL(8) :: r( * )
    INTEGER(4) :: rc


    INTEGER :: i
    INTEGER :: j

    TYPE(base(8,:,:)), ALLOCATABLE :: array( : )
    TYPE(base(8,:,:)), POINTER :: iPtr


    array = [   [ i1, i2, i3 ],&
                [   base(8,3,3)('IBM',r( 1:3 )),&
                    base(8,3,3)('XLF',r( 4:6 )),&
                    base(8,3,3)('TST',r( 7:9 ))    ],&
                [ i1, i2, i3 ] ]


    IF (SIZE( array ) /= 9)         CALL zzrc( rc )

    DO i = 1, SIZE( array )
        j = MOD((i - 1), 3) + 1

        iPtr => i1
        IF (j == 2) THEN
            iPtr => i2
        ELSE IF (j > 2) THEN
            iPtr => i3
        END IF

        IF (array( i ) /= iPtr)    CALL zzrc( (rc + INT(i, 4)) )
    END DO

END SUBROUTINE BaseElementsAsArguments
