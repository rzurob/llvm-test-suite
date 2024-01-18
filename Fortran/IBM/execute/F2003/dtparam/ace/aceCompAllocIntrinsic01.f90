!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : aceCompAllocIntrinsic01
!*                               with DTP
!*
!*  DATE                       : November 17, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor contains a Derived Type
!*                               with an ALLOCATABLE Component
!*  SECONDARY FUNCTIONS TESTED : and is an argument to an Intrinsic Function
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  1)  Testing the format of the Array Constructor:
!*      * Brackets:  [ ]
!*      * Where the ac-value-list contains:
!*        o A Derived Type with an ALLOCATABLE Non-Polymorphic
!*              Component
!*
!*  2)  Testing the usage of an Array Constructor in various contexts:
!*      * As an argument to the ANY(), ALL(), and RESHAPE() Intrinsic
!*        Functions
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE bMod

    IMPLICIT NONE

    TYPE base(k1,l1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        COMPLEX(k1), ALLOCATABLE :: cCompA( : )
        CHARACTER(l1) :: lable

        CONTAINS

            GENERIC :: OPERATOR(==) => Equal2
            GENERIC :: OPERATOR(/=) => NotEqual2
            PROCEDURE, PASS :: Equal2 => Base2Equal
            PROCEDURE, PASS :: NotEqual2 => Base2NotEqual

    END TYPE base

    CONTAINS

        ELEMENTAL LOGICAL FUNCTION Base2Equal(this, o)
            CLASS(base(8,*)), INTENT(in) :: this
            CLASS(base(8,*)), INTENT(in) :: o


            Base2Equal = (.NOT. (this /= o))

        END FUNCTION Base2Equal

        ELEMENTAL LOGICAL FUNCTION Base2NotEqual(this, o)
            CLASS(base(8,*)), INTENT(in) :: this
            CLASS(base(8,*)), INTENT(in) :: o


            Base2NotEqual = .TRUE.
            IF ((this%l1 == o%l1)       .AND.&
                (this%k1 == o%k1)       .AND.&
                (this%lable == o%lable) .AND.&
                ( ALL(this%cCompA == o%cCompA) )) THEN
                Base2NotEqual = .FALSE.
            END IF

        END FUNCTION Base2NotEqual

END MODULE bMod


PROGRAM aceCompAllocIntrinsic01
    USE bMod

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j
    INTEGER :: k

    REAL(8), PARAMETER :: rA( 36 ) = [ ((1.0_8 / REAL(i, 8)), i = 1, 36) ]
    COMPLEX(8) :: cA( 18 ) =&
                    [ (CMPLX(rA( i ), rA( (i + 1) ), 8), i = 1, 36, 2) ]

    TYPE(base(8,3)) :: bA( 3 )
    TYPE(base(8,3)) :: bA2( 2,2 )


    DO i = 1, 3
        j = ((i - 1) * 6) + 1
        WRITE(bA( i )%lable, '(I3.3)') i
        bA( i )%cCompA = cA( j:(j + 5) )
    END DO


    IF ( ANY(bA /=                                                  &
            [   base(8,3)([ cA( 1:6 ) ],'001'),                     &
                base(8,3)(cA( 7:12 ),'002'),                        &
                base(8,3)([ cA( 13 ), cA( 14 ), cA( 15 ), cA( 16 ), &
                                CMPLX(rA( 33 ),rA( 34 ),8),         &
                                CMPLX(rA( 35 ),rA( 36 ),8) ],'003') ]) ) THEN
        STOP 10
    END IF


    IF (.NOT. ALL(  [   base(8,3)(cA( 6:1:-1 ),'ABC'),          &
                        base(8,3)([ cA( 12:7:-1 ) ],'DEF') ]    &
                                ==                              &
                    [   base(8,3)(cA( 6:1:-1 ),'ABC'),          &
                        base(8,3)([ cA( 12:7:-1 ) ],'DEF') ]))  THEN
        STOP 20
    END IF



    DO i = 1, 2
        DO j = 1, 2
            k = MOD((((i - 1) * 2) + j), 3) + 1
            bA2( j,i ) = bA( k )
        END DO
    END DO


    IF ( ANY(bA2 /=&
            RESHAPE([ bA( 2 ), bA( 3 ), bA( 1 ), bA( 2 ) ], [ 2,2 ])) ) THEN
        STOP 30
    END IF

END PROGRAM aceCompAllocIntrinsic01
