!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : aceScalarVariableExpr02
!*  TEST CASE TITLE            : Basic (non-Polymorphic) Array Constructors
!*                               with DTP
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : November  7, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor contains Scalar Variables
!*                               of Derived Type (with Assumed Type Parameters)
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
!*      * Brackets:  (/ /)
!*      * Where the ac-value-list contains:
!*            o Scalar Variables (with Assumed Length Parameters),
!*
!*  2)  Testing the usage of an Array Constructor in various contexts:
!*      * As the expr of an Intrinsic Assignment,
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE bMod

    IMPLICIT NONE

    TYPE base(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        INTEGER(k1) :: code
        CHARACTER(l1) :: lable

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual2, NotEqual8
            PROCEDURE, PASS :: NotEqual2 => Base2NotEqual
            PROCEDURE, PASS :: NotEqual8 => Base8NotEqual

    END TYPE base

    CONTAINS

        LOGICAL FUNCTION Base2NotEqual(this, o)
            CLASS(base(2,*)), INTENT(in) :: this
            CLASS(base(2,*)), INTENT(in) :: o


            Base2NotEqual = .TRUE.
            IF ((this%l1 == o%l1)       .AND.&
                (this%code == o%code)   .AND.&
                (this%lable == o%lable)) THEN
                Base2NotEqual = .FALSE.
            END IF

        END FUNCTION Base2NotEqual

        LOGICAL FUNCTION Base8NotEqual(this, o)
            CLASS(base(8,*)), INTENT(in) :: this
            CLASS(base(8,*)), INTENT(in) :: o


            Base8NotEqual = .TRUE.
            IF ((this%l1 == o%l1)       .AND.&
                (this%code == o%code)   .AND.&
                (this%lable == o%lable)) THEN
                Base8NotEqual = .FALSE.
            END IF

        END FUNCTION Base8NotEqual

        SUBROUTINE BaseElementsAsArguments(i1, i2, i3, rc)
            IMPLICIT NONE

            TYPE(base(2,*)) :: i1
            TYPE(base(2,*)) :: i2
            TYPE(base(2,*)) :: i3
            INTEGER(4) :: rc

            TYPE(base(2,:)), ALLOCATABLE :: array( : )


            array = (/ i1, i2, i3 /)

            IF ( ANY(SHAPE( array ) /= (/ 3 /)) )   CALL zzrc( rc )
            IF (array( 1 ) /= i1)                   CALL zzrc( (rc + 1_4) )
            IF (array( 2 ) /= i2)                   CALL zzrc( (rc + 2_4) )
            IF (array( 3 ) /= i3)                   CALL zzrc( (rc + 3_4) )

        END SUBROUTINE BaseElementsAsArguments

END MODULE bMod


MODULE eMod
    USE bMod

    IMPLICIT NONE

    TYPE, EXTENDS(base) :: extended(l2)
        INTEGER, LEN :: l2

        REAL(k1) :: array( l2 )

        CONTAINS

            PROCEDURE, PASS :: NotEqual8 => Extended8NotEqual

    END TYPE extended

    CONTAINS

        LOGICAL FUNCTION Extended8NotEqual(this, o)
            CLASS(extended(8,*,*)), INTENT(in) :: this
            CLASS(base(8,*)), INTENT(in) :: o


            Extended8NotEqual = .TRUE.
            IF ((this%l1 == o%l1)       .AND.&
                (this%code == o%code)   .AND.&
                (this%lable == o%lable)) THEN
                SELECT TYPE (o)
                    TYPE IS (extended(8,*,*))
                        IF ((this%l2 == o%l2)   .AND.&
                            ( ALL(this%array == o%array) )) THEN
                            Extended8NotEqual = .FALSE.
                        END IF
                END SELECT
            END IF

        END FUNCTION Extended8NotEqual

END MODULE eMod


PROGRAM aceScalarVariableExpr02
    USE eMod

    IMPLICIT NONE

    INTERFACE
        SUBROUTINE ExtendedElementsAsArguments(i1, i2, i3, rc)
            USE eMod

            IMPLICIT NONE

            TYPE(extended(8,3,3)) :: i1
            TYPE(extended(8,3,3)) :: i2
            TYPE(extended(8,3,3)) :: i3
            INTEGER(4) :: rc
        END SUBROUTINE ExtendedElementsAsArguments
    END INTERFACE


    INTEGER :: i

    REAL(8) :: rA( 9 ) = (/ ((1.0_8 / REAL(i, 8)), i = 1, 9) /)

    TYPE(base(2,6)) :: baseItem1
    TYPE(base(2,6)) :: baseItem2
    TYPE(base(2,6)) :: baseItem3

    TYPE(extended(8,3,3)) :: extendedItem1
    TYPE(extended(8,3,3)) :: extendedItem2
    TYPE(extended(8,3,3)) :: extendedItem3


    baseItem1 = base(2,6)(1_2,'Item-1')
    baseItem2 = base(2,6)(2_2,'Item-2')
    baseItem3 = base(2,6)(3_2,'Item-3')

    CALL BaseElementsAsArguments(baseItem1, baseItem2, baseItem3, 20_4)


    extendedItem1 = extended(8,3,3)(1_8,'v.1',rA(  1:3  ))
    extendedItem2 = extended(8,3,3)(2_8,'v.2',rA(  4:6  ))
    extendedItem3 = extended(8,3,3)(3_8,'v.3',rA(  7:9  ))

    CALL ExtendedElementsAsArguments(extendedItem1, extendedItem2,&
                                                    extendedItem3, 40_4)

END PROGRAM aceScalarVariableExpr02


SUBROUTINE ExtendedElementsAsArguments(i1, i2, i3, rc)
    USE eMod

    IMPLICIT NONE

    TYPE(extended(8,3,3)) :: i1
    TYPE(extended(8,3,3)) :: i2
    TYPE(extended(8,3,3)) :: i3
    INTEGER(4) :: rc


    TYPE(extended(8,:,:)), ALLOCATABLE :: array( : )


    array = (/ i1, i2, i3 /)

    IF ( ANY(SHAPE( array ) /= (/ 3 /)) )   CALL zzrc( rc )
    IF (array( 1 ) /= i1)                   CALL zzrc( (rc + 1_4) )
    IF (array( 2 ) /= i2)                   CALL zzrc( (rc + 2_4) )
    IF (array( 3 ) /= i3)                   CALL zzrc( (rc + 3_4) )

END SUBROUTINE ExtendedElementsAsArguments
