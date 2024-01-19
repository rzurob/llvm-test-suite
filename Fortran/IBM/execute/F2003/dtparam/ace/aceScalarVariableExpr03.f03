!***********************************************************************
!* =====================================================================
!*
!*                               with DTP
!*
!*  DATE                       : November 11, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor contains Scalar Variables
!*                               of Derived Type (with Deferred Type Parameters)
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
!*      * Brackets:  (/ /)
!*      * Where the ac-value-list contains:
!*            o Scalar Variables (with Deferred Length Parameters),
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

            GENERIC :: OPERATOR(/=) => NotEqual2
            PROCEDURE, PASS :: NotEqual2 => Base2NotEqual

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

END MODULE bMod


MODULE cMod
    USE bMod

    IMPLICIT NONE

    TYPE container(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        REAL(k1) :: array( (l1 + 1) )
        TYPE(base((k1 / 4),(l1 * 2))) :: b

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual8
            PROCEDURE, PASS :: NotEqual8 => Container8NotEqual

    END TYPE container

    CONTAINS

        LOGICAL FUNCTION Container8NotEqual(this, o)
            CLASS(container(*,8)), INTENT(in) :: this
            CLASS(container(*,8)), INTENT(in) :: o


            Container8NotEqual = .TRUE.
            IF ((this%l1 == o%l1)               .AND.&
                (this%k1 == o%k1)               .AND.&
                ( ALL(this%array == o%array) )  .AND.&
                (.NOT. (this%b /= o%b))) THEN

                Container8NotEqual = .FALSE.

            END IF

        END FUNCTION Container8NotEqual

END MODULE cMod


PROGRAM aceScalarVariableExpr03
    USE cMod

    IMPLICIT NONE

    INTEGER :: i

    REAL(8) :: rA( 4 ) = (/ ((1.0_8 / REAL(i, 8)), i = 1, 4) /)

    TYPE(base(2,:)), ALLOCATABLE :: baseItem1
    TYPE(base(2,:)), ALLOCATABLE :: baseItem2
    TYPE(base(2,:)), ALLOCATABLE :: baseItem3

    TYPE(base(2,:)), ALLOCATABLE :: baseArray( : )

    TYPE(container(:,8)), POINTER :: containerItem1
    TYPE(container(:,8)), POINTER :: containerItem2
    TYPE(container(:,8)), POINTER :: containerItem3

    TYPE(container(:,8)), ALLOCATABLE :: containerArray( : )


    baseItem1 = base(2,6)(1_2,'Item-1')
    baseItem2 = base(2,6)(2_2,'Item-2')
    baseItem3 = base(2,6)(3_2,'Item-3')

    baseArray = (/ baseItem1, baseItem2, baseItem3 /)

    IF ( ANY(SHAPE( baseArray ) /= (/ 3 /)) )   CALL zzrc( 10_4 )
    IF (baseArray( 1 ) /= baseItem1)            CALL zzrc( 11_4 )
    IF (baseArray( 2 ) /= baseItem2)            CALL zzrc( 12_4 )
    IF (baseArray( 3 ) /= baseItem3)            CALL zzrc( 13_4 )


    ALLOCATE(containerItem1, SOURCE=container(3,8)(rA,baseItem1))

    containerItem2 => containerItem1
    containerItem3 => containerItem2

    containerArray = (/ containerItem1, containerItem2, containerItem3 /)

    IF ( ANY(SHAPE( containerArray ) /= (/ 3 /)) )      CALL zzrc( 20_4 )
    IF (containerArray( 1 ) /= containerItem1)          CALL zzrc( 21_4 )
    IF (containerArray( 2 ) /= containerItem2)          CALL zzrc( 22_4 )
    IF (containerArray( 3 ) /= containerItem3)          CALL zzrc( 23_4 )

END PROGRAM aceScalarVariableExpr03
