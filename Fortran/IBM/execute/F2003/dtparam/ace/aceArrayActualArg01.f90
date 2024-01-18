!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : aceArrayActualArg01
!*                               with DTP
!*
!*  DATE                       : November 12, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor contains Array Variables
!*                               of Derived Type (with Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : and the Array Constructor is used as the
!*                               Actual Argument to a SUBROUTINE
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
!*        o Scalar Variables,
!*        o Array Variables (with Assumed Length Parameters)
!*
!*  2)  Testing the usage of an Array Constructor in various contexts:
!*      * As the Actual Argument in a SUBROUTINE call
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE bMod

    IMPLICIT NONE

    TYPE base(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        CHARACTER(l1) :: lable
        REAL(k1), ALLOCATABLE :: array( : )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual8
            PROCEDURE, PASS :: NotEqual8 => Base8NotEqual

    END TYPE base

    CONTAINS

        LOGICAL FUNCTION Base8NotEqual(this, o)
            CLASS(base(*,8)), INTENT(in) :: this
            CLASS(base(*,8)), INTENT(in) :: o


            Base8NotEqual = .TRUE.
            IF ((this%k1 == o%k1)  .AND.&
                (this%l1 == o%l1)  .AND.&
                (this%lable == o%lable)  .AND.&
                ( ALLOCATED( o%array ) ) .AND.&
                ( ALLOCATED( this%array ) ) .AND.&
                ( ALL(this%array == o%array) )) THEN
                Base8NotEqual = .FALSE.
            END IF

        END FUNCTION Base8NotEqual

END MODULE bMod


PROGRAM aceArrayActualArg01
    USE bMod

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j

    INTEGER :: idx( 10 ) = [ (i, i = 1, 10) ]
    REAL(8) :: rA( 30 ) = [ ((1.0_8 / REAL(i, 8)), i = 1, 30) ]

    TYPE(base(5,8)) :: baseA( 10 )


    DO i = 1, 10
        j = ((i - 1) * 3) + 1
        baseA( i )%array = rA( j:(j + 2) )

        WRITE(baseA( i )%lable, '("Item",I1)') (i - 1)
    END DO

    CALL CheckArrayArgument(&
            [   baseA( 1 ), baseA( 2 ), baseA( 3 ),&
                baseA( 4 ), baseA( 5 ), baseA( 6 ),&
                baseA( 7 ), baseA( 8 ), baseA( 9 ), baseA( 10 ) ], 10, 10_4)

    CONTAINS

        !
        ! Algorithm:
        ! - Verify the contents of the Array Argument,
        ! - Reduce Array Size by 2 Elements (remove the 2 middle elements),
        ! - Swap upper and lower Array Sections,
        ! - Repeat until the new Array Size is 0.
        !
        ! Thus, where:
        ! - "a" is the lower Array Section,
        ! - "b" is the 2 middle elements, and
        ! - "c" is the upper Array Section.
        !
        ! [ a, b, c ] => [ c, a ]
        !
        ! Or, more literally:
        !
        ! 1) [ 1, 2, 3, 4,  5, 6,  7, 8, 9, 10 ] => [ 7, 8, 9, 10,  1, 2, 3, 4 ]
        ! 2) [ 7, 8, 9,    10, 1,      2, 3, 4 ] => [ 2, 3, 4,         7, 8, 9 ]
        ! 3) [ 2, 3,        4, 7,         8, 9 ] => [ 8, 9,               2, 3 ]
        ! 4) [ 8,           9, 2,            3 ] => [ 3,                     8 ]
        ! 5) [ 3,                            8 ] => [  {Recursion Stops Here}  ]
        !
        RECURSIVE SUBROUTINE CheckArrayArgument(a, s, rc)
            TYPE(base(*,8)) :: a( : )
            INTEGER :: s
            INTEGER(4) :: rc

            INTEGER :: i
            INTEGER :: j
            INTEGER :: k
            INTEGER :: sNew
            INTEGER :: idxNew( (s - 2) )


            IF (SIZE( a ) /= s)                    CALL zzrc( rc )

            DO i = 1, SIZE( a )
                IF (a( i ) /= baseA( idx( i ) ))   CALL zzrc( (rc + INT(i, 4)) )
            END DO


            sNew = s - 2
            IF (sNew > 0) THEN
                i = sNew / 2
                j = s - i + 1

                idxNew( :i ) = [ (idx( k ), k = j, s) ]
                idxNew( (i + 1): ) = [ (idx( k ), k = 1, i) ]

                idx = -1
                idx( :sNew ) = [ (idxNew( k ), k = 1, sNew) ]

                CALL CheckArrayArgument([ a( j: ), a( :i ) ],sNew,(rc + 20_4))
            END IF

        END SUBROUTINE CheckArrayArgument

END PROGRAM aceArrayActualArg01
