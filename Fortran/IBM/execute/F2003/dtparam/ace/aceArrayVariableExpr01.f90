!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : aceArrayVariableExpr01
!*  TEST CASE TITLE            : Basic (non-Polymorphic) Array Constructors
!*                               with DTP
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : November 11, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor contains Array Variables
!*                               of Derived Type (with Type Parameters)
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
!*            o Array Variables (with Assumed Length Parameters),
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

        INTEGER :: code
        REAL(k1) :: array( l1 )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual4, NotEqual8
            PROCEDURE, PASS :: NotEqual4 => Base4NotEqual
            PROCEDURE, PASS :: NotEqual8 => Base8NotEqual

    END TYPE base

    CONTAINS

        LOGICAL FUNCTION Base4NotEqual(this, o)
            CLASS(base(4,*)), INTENT(in) :: this
            CLASS(base(4,*)), INTENT(in) :: o


            Base4NotEqual = .TRUE.
            IF ((this%code == o%code)  .AND.&
                ( ALL(this%array == o%array) )) THEN
                Base4NotEqual = .FALSE.
            END IF

        END FUNCTION Base4NotEqual

        LOGICAL FUNCTION Base8NotEqual(this, o)
            CLASS(base(8,*)), INTENT(in) :: this
            CLASS(base(8,*)), INTENT(in) :: o


            Base8NotEqual = .TRUE.
            IF ((this%code == o%code)  .AND.&
                ( ALL(this%array == o%array) )) THEN
                Base8NotEqual = .FALSE.
            END IF

        END FUNCTION Base8NotEqual

END MODULE bMod


PROGRAM aceArrayVariableExpr01
    USE bMod

    IMPLICIT NONE

    INTERFACE
        SUBROUTINE BaseElementsAsArguments(i1, i2, i3, rc)
            USE bMod

            IMPLICIT NONE

            TYPE(base(4,*)), TARGET :: i1( : )
            TYPE(base(4,*)), TARGET :: i2( : )
            TYPE(base(4,*)), TARGET :: i3( : )
            INTEGER(4) :: rc
        END SUBROUTINE BaseElementsAsArguments
    END INTERFACE

    INTEGER :: i
    INTEGER :: j

    REAL(4) :: rA( 9 ) = [ ((1.0_4 / REAL(i, 4)), i = 1, 9) ]

    TYPE(base(4,3)), TARGET :: baseItem1( 3 )
    TYPE(base(4,3)), TARGET :: baseItem2( 3 )
    TYPE(base(4,3)), TARGET :: baseItem3( 3 )

    TYPE(base(4,3)), POINTER :: baseItemPtr( : )

    TYPE(base(4,3)) :: baseA( 9 )


    DO i = 1, 3
        j = ((i - 1) * 3) + 1

        baseItem1( i ) = base(4,3)(i,rA( j:(j + 2) ))
        baseItem2( i ) = base(4,3)(i,rA( j:(j + 2) ))
        baseItem3( i ) = base(4,3)(i,rA( j:(j + 2) ))
    END DO


    baseA = [ baseItem1, baseItem2, baseItem3 ]

    DO i = 1, 9
        baseItemPtr => baseItem1
        IF (i > 3) baseItemPtr => baseItem2
        IF (i > 6) baseItemPtr => baseItem3

        j = MOD((i - 1), 3) + 1

        IF (baseA( i ) /= baseItemPtr( j ))    CALL zzrc( (10_4 + INT(i, 4)) )
    END DO


    CALL BaseElementsAsArguments(baseItem1, baseItem2, baseItem3, 20_4)

END PROGRAM aceArrayVariableExpr01


SUBROUTINE BaseElementsAsArguments(i1, i2, i3, rc)
    USE bMod

    IMPLICIT NONE

    TYPE(base(4,*)), TARGET :: i1( : )
    TYPE(base(4,*)), TARGET :: i2( : )
    TYPE(base(4,*)), TARGET :: i3( : )
    INTEGER(4) :: rc


    INTEGER :: i
    INTEGER :: j

    TYPE(base(4,:)), ALLOCATABLE :: array( : )
    TYPE(base(4,:)), POINTER :: itemPtr( : )


    array = [ i1, i2, i3 ]

    DO i = 1, 9
        itemPtr => i1
        IF (i > 3) itemPtr => i2
        IF (i > 6) itemPtr => i3

        j = MOD((i - 1), 3) + 1

        IF (array( i ) /= itemPtr( j ))    CALL zzrc( (rc + INT(i, 4)) )
    END DO

END SUBROUTINE BaseElementsAsArguments
