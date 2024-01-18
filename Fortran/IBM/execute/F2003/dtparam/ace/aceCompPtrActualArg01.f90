!***********************************************************************
!* =====================================================================
!*
!*                               with DTP
!*
!*  DATE                       : November 25, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor contains a Derived Type
!*                               with a Non-Polymorphic POINTER Component
!*  SECONDARY FUNCTIONS TESTED : and is the Acutal Argument to a SUBROUTINE
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
!*        o A Derived Type with a POINTER Component (Non-Polymorphic)
!*
!*  2)  Testing the usage of an Array Constructor in various contexts:
!*      * As the Actual Argument in a User Defined SUBROUTINE call,
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE typeMod

    IMPLICIT NONE

    TYPE :: baseType(l1)
        INTEGER, LEN :: l1

        CHARACTER(l1) :: str

        CONTAINS

            GENERIC :: OPERATOR(/=) => BaseTypeNotEqual
            PROCEDURE, PASS :: BaseTypeNotEqual

    END TYPE baseType

    CONTAINS

        ELEMENTAL LOGICAL FUNCTION BaseTypeNotEqual(this, o)
            CLASS(baseType(*)), INTENT(in) :: this
            CLASS(baseType(*)), INTENT(in) :: o


            BaseTypeNotEqual = .TRUE.
            IF ((this%l1 == o%l1)    .AND.&
                (this%str == o%str)) THEN
                BaseTypeNotEqual = .FALSE.
            END IF

        END FUNCTION BaseTypeNotEqual

END MODULE typeMod


MODULE baseMod
    USE typeMod

    IMPLICIT NONE

    TYPE base(l1,k1,l2)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1
        INTEGER, LEN :: l2

        TYPE(baseType(l2)), POINTER :: bT( : )
        REAL(k1) :: array( l1 )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual
            PROCEDURE, PASS :: NotEqual => BaseNotEqual

    END TYPE base

    CONTAINS

        LOGICAL FUNCTION BaseNotEqual(this, o)
            CLASS(base(*,16,*)), INTENT(in) :: this
            CLASS(base(*,16,*)), INTENT(in) :: o


            BaseNotEqual = .TRUE.
            IF ((this%l1 == o%l1)               .AND.&
                (this%l2 == o%l2)               .AND.&
                ( ANY(this%array == o%array) )  .AND.&
                (.NOT. ANY(this%bT /= o%bT))) THEN
                BaseNotEqual = .FALSE.
            END IF

        END FUNCTION BaseNotEqual

END MODULE baseMod


PROGRAM aceCompPtrActualArg01
    USE baseMod

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j
    INTEGER :: k

    REAL(16) :: r16A( 27 ) = [ (1.0_16 / REAL(i, 16), i = 1, 27) ]

    CHARACTER(5) :: items( 27 )

    TYPE(baseType(5)) :: bT( 27 )
    TYPE(base(3,16,5)) :: b( 3,3 )


    DO i = 1, 27
        items( i ) = 'Item' // CHAR( (48 + i) )
        bT( i )%str = items( i )
    END DO


    DO i = 1, 3
        DO j = 1, 3
            k = ((i - 1) * 3) + j
            k = ((k - 1) * 3) + 1
            b( j,i )%array = r16A( k:(k + 2) )
            ALLOCATE(b( j,i )%bT( 3 ), SOURCE=[ bT( k:(k + 2) ) ])
        END DO
    END DO


    CALL BuildArray(b( 3,3 ), b( 2,3 ), b( 1,3 ),&
                    b( 3,2 ), b( 2,2 ), b( 1,2 ),&
                    b( 3,1 ), b( 2,1 ), b( 1,1 ))

    CONTAINS

        SUBROUTINE BuildArray(i1, i2, i3, i4, i5, i6, i7, i8, i9)
            TYPE(base(*,16,*)) :: i1, i2, i3, i4, i5, i6, i7, i8, i9

            CALL CheckArray(&
                RESHAPE([ i1, i2, i3, i4, i5, i6, i7, i8, i9 ], [ 3,3 ]) )

        END SUBROUTINE BuildArray

        SUBROUTINE CheckArray( theArray )
            TYPE(base(*,16,*)) :: theArray( :,: )

            INTEGER :: i, j, k, l, m

            k = 0
            DO i = 3, 1, -1
                k = k + 1

                l = 0
                DO j = 3, 1, -1
                    l = l + 1
                    m = ((i - 1) * 3) + j
                    m = ((m - 1) * 3) + 1

                    IF (theArray( j,i ) /= b( l,k )) THEN
                        CALL zzrc( 10_4 + INT(m, 4) )
                    END IF
                END DO
            END DO

        END SUBROUTINE CheckArray

END PROGRAM aceCompPtrActualArg01
