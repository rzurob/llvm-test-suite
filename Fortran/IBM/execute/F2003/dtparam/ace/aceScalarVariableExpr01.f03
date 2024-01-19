!***********************************************************************
!* =====================================================================
!*
!*                               with DTP
!*
!*  DATE                       : November  7, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor contains Scalar Variables
!*                               of Derived Type (with Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : and is the expr of an Intrinsic Assignment
!*
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
!*            o Scalar Variables,
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
            IF ((this%code == o%code)  .AND.&
                (this%lable == o%lable)) THEN
                Base2NotEqual = .FALSE.
            END IF

        END FUNCTION Base2NotEqual

        LOGICAL FUNCTION Base8NotEqual(this, o)
            CLASS(base(8,*)), INTENT(in) :: this
            CLASS(base(8,*)), INTENT(in) :: o


            Base8NotEqual = .TRUE.
            IF ((this%code == o%code)  .AND.&
                (this%lable == o%lable)) THEN
                Base8NotEqual = .FALSE.
            END IF

        END FUNCTION Base8NotEqual

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
            IF ((this%code == o%code)  .AND.&
                (this%lable == o%lable)) THEN
                SELECT TYPE (o)
                    TYPE IS (extended(8,*,*))
                        IF ( ALL(this%array == o%array) ) THEN
                            Extended8NotEqual = .FALSE.
                        END IF
                END SELECT
            END IF

        END FUNCTION Extended8NotEqual

        SUBROUTINE ExtendedElementsAsArguments(i1, i2, i3, rc)
            IMPLICIT NONE

            TYPE(extended(8,3,3)) :: i1
            TYPE(extended(8,3,3)) :: i2
            TYPE(extended(8,3,3)) :: i3
            INTEGER(4) :: rc


            TYPE(extended(8,3,3)) :: array( 3 )


            array = [ i1, i2, i3 ]

            IF (array( 1 ) /= i1)       CALL zzrc( (rc + 1_4) )
            IF (array( 2 ) /= i2)       CALL zzrc( (rc + 2_4) )
            IF (array( 3 ) /= i3)       CALL zzrc( (rc + 3_4) )

        END SUBROUTINE ExtendedElementsAsArguments

END MODULE eMod


PROGRAM aceScalarVariableExpr01
    USE eMod

    IMPLICIT NONE

    INTERFACE
        SUBROUTINE BaseElementsAsArguments(i1, i2, i3, rc)
            USE eMod

            IMPLICIT NONE

            TYPE(base(2,6)) :: i1
            TYPE(base(2,6)) :: i2
            TYPE(base(2,6)) :: i3
            INTEGER(4) :: rc
        END SUBROUTINE BaseElementsAsArguments
    END INTERFACE

    INTEGER :: i

    REAL(8) :: rA( 15 ) = [ ((1.0_8 / REAL(i, 8)), i = 1, 15) ]

    TYPE(base(2,6)) :: baseItem1
    TYPE(base(2,6)) :: baseItem2
    TYPE(base(2,6)) :: baseItem3
    TYPE(base(2,6)) :: baseItem4
    TYPE(base(2,6)) :: baseItem5

    TYPE(base(2,6)) :: baseA( 5 )

    TYPE(extended(8,3,3)) :: extendedItem1
    TYPE(extended(8,3,3)) :: extendedItem2
    TYPE(extended(8,3,3)) :: extendedItem3
    TYPE(extended(8,3,3)) :: extendedItem4
    TYPE(extended(8,3,3)) :: extendedItem5

    TYPE(extended(8,3,3)) :: extendedA( 5 )


    baseItem1 = base(2,6)(1_2,'Item-1')
    baseItem2 = base(2,6)(2_2,'Item-2')
    baseItem3 = base(2,6)(3_2,'Item-3')
    baseItem4 = base(2,6)(4_2,'Item-4')
    baseItem5 = base(2,6)(5_2,'Item-5')

    baseA = [ baseItem1, baseItem2, baseItem3, baseItem4, baseItem5 ]

    IF (baseA( 1 ) /= baseItem1)    CALL zzrc( 11_4 )
    IF (baseA( 2 ) /= baseItem2)    CALL zzrc( 12_4 )
    IF (baseA( 3 ) /= baseItem3)    CALL zzrc( 13_4 )
    IF (baseA( 4 ) /= baseItem4)    CALL zzrc( 14_4 )
    IF (baseA( 5 ) /= baseItem5)    CALL zzrc( 15_4 )

    CALL BaseElementsAsArguments(baseItem1, baseItem3, baseItem5, 20_4)


    extendedItem1 = extended(8,3,3)(1_8,'v.1',rA(  1:3  ))
    extendedItem2 = extended(8,3,3)(2_8,'v.2',rA(  4:6  ))
    extendedItem3 = extended(8,3,3)(3_8,'v.3',rA(  7:9  ))
    extendedItem4 = extended(8,3,3)(4_8,'v.4',rA( 10:12 ))
    extendedItem5 = extended(8,3,3)(5_8,'v.5',rA( 13:15 ))

    extendedA = [   extendedItem1, extendedItem2,&
                    extendedItem3, extendedItem4, extendedItem5 ]

    IF (extendedA( 1 ) /= extendedItem1)    CALL zzrc( 31_4 )
    IF (extendedA( 2 ) /= extendedItem2)    CALL zzrc( 32_4 )
    IF (extendedA( 3 ) /= extendedItem3)    CALL zzrc( 33_4 )
    IF (extendedA( 4 ) /= extendedItem4)    CALL zzrc( 34_4 )
    IF (extendedA( 5 ) /= extendedItem5)    CALL zzrc( 35_4 )

    CALL ExtendedElementsAsArguments(extendedItem1, extendedItem2,&
                                                    extendedItem3, 40_4)

END PROGRAM aceScalarVariableExpr01


SUBROUTINE BaseElementsAsArguments(i1, i2, i3, rc)
    USE eMod

    IMPLICIT NONE

    TYPE(base(2,6)) :: i1
    TYPE(base(2,6)) :: i2
    TYPE(base(2,6)) :: i3
    INTEGER(4) :: rc


    TYPE(base(2,6)) :: array( 3 )


    array = [ i1, i2, i3 ]

    IF (array( 1 ) /= i1)       CALL zzrc( (rc + 1_4) )
    IF (array( 2 ) /= i2)       CALL zzrc( (rc + 2_4) )
    IF (array( 3 ) /= i3)       CALL zzrc( (rc + 3_4) )

END SUBROUTINE BaseElementsAsArguments
