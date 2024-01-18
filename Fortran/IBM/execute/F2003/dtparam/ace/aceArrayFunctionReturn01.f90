!***********************************************************************
!* =====================================================================
!*
!*                               with DTP
!*
!*  DATE                       : November  7, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor contains Array Variable(s)
!*                               of Derived Type (with Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : and is the value for a User Defined FUNCTION
!*                               Result
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
!*            o Scalar Variables.
!*
!*  2)  Testing the usage of an Array Constructor in various contexts:
!*      * As the expr of an Intrinsic Assignment, and
!*      * As a FUNCTION Return value.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE bMod

    IMPLICIT NONE

    TYPE base(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        INTEGER(k1) :: idx
        COMPLEX(k1) :: array( l1 )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual8
            PROCEDURE, PASS :: NotEqual8 => Base8NotEqual

            PROCEDURE, NOPASS :: Factorial

    END TYPE base

    CONTAINS

        LOGICAL FUNCTION Base8NotEqual(this, o)
            CLASS(base(*,8)), INTENT(in) :: this
            CLASS(base(*,8)), INTENT(in) :: o


            Base8NotEqual = .TRUE.
            IF ((this%idx == o%idx) .AND.&
                ( ALL(this%array == o%array) )) THEN
                Base8NotEqual = .FALSE.
            END IF

        END FUNCTION Base8NotEqual

        RECURSIVE FUNCTION Factorial( o ) RESULT( factorialResult )
            TYPE(base(*,8)) :: o( : )

            TYPE(base(:,8)), ALLOCATABLE :: factorialResult( : )

            IF (SIZE( o ) == 1) THEN
                factorialResult = o

            ELSE
                factorialResult = (/ o, o%Factorial( o( :(SIZE( o ) - 1) ) ) /)
            END IF

        END FUNCTION Factorial

END MODULE bMod


PROGRAM aceArrayFunctionReturn01
    USE bMod

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j

    REAL(8), PARAMETER :: rA( 18 ) = (/ ((1.0_8 / REAL(i, 8)), i = 1, 18) /)
    COMPLEX(8), PARAMETER :: cA( 9 ) =&
        (/ (CMPLX(rA( i ),rA( (i + 1) )), i = 1, 18, 2) /)

    INTEGER(8) :: expected( 6 ) = (/ 1_8, 2_8, 3_8, 1_8, 2_8, 1_8 /)


    TYPE(base(3,8)), PARAMETER :: baseItem1 = base(3,8)(1,cA( 1:3 ))
    TYPE(base(3,8)), PARAMETER :: baseItem2 = base(3,8)(2,cA( 4:6 ))
    TYPE(base(3,8)), PARAMETER :: baseItem3 = base(3,8)(3,cA( 7:9 ))

    TYPE(base(3,8)) :: baseInA( 3 )
    TYPE(base(:,8)), ALLOCATABLE :: baseOutA( : )


    baseInA = (/ baseItem1, baseItem2, baseItem3 /)
    baseOutA = baseInA(1)%Factorial( baseInA )


    IF (SIZE( baseOutA ) /= 6)              CALL zzrc( 10_4 )
    IF ( ANY(baseOutA%idx /= expected) )    CALL zzrc( 11_4 )


    DO i = 1, SIZE( baseOutA )
        j = ((expected( i ) - 1) * 3) + 1
        IF ( ANY(baseOutA( i )%array /= cA( j:(j + 2) )) )&
                                        CALL zzrc( 20_4 + INT(i, 4) )
    END DO

END PROGRAM aceArrayFunctionReturn01
