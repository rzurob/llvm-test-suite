!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : aceStrConstrSelector01
!*                               with DTP
!*
!*  DATE                       : November 13, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor contains Structure
!*                               Constructors (with Type Parameters)
!*  SECONDARY FUNCTIONS TESTED : and the Array Constructor is used as the
!*                               selector in an ASSOCIATE Construct
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
!*        o Structure Constructors,
!*
!*  2)  Testing the usage of an Array Constructor in various contexts:
!*      * As the selector of an ASSOCIATE Construct
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE bMod

    IMPLICIT NONE

    TYPE b(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        REAL(k1) :: a( (l1 + 1) )

        CONTAINS

            GENERIC :: OPERATOR(/=) => NotEqual16
            PROCEDURE, PASS :: NotEqual16 => b16NotEqual

    END TYPE b

    CONTAINS

        LOGICAL FUNCTION b16NotEqual(this, o)
            CLASS(b(*,16)), INTENT(in) :: this
            CLASS(b(*,16)), INTENT(in) :: o


            b16NotEqual = .TRUE.
            IF ((this%k1 == o%k1)  .AND.&
                (this%l1 == o%l1)  .AND.&
                ( ALL(this%a == o%a) )) THEN
                b16NotEqual = .FALSE.
            END IF

        END FUNCTION b16NotEqual

END MODULE bMod


PROGRAM aceStrConstrSelector01
    USE bMod

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j

    REAL(16) :: rA( 4 ) = [ ((1.0_16 / REAL(i, 16)), i = 1, 4) ]


    TYPE(b(3,16)) :: bA( 3 )


    DO i = 1, 3
        bA( i ) = b(3,16)(rA)
    END DO


    ASSOCIATE(array => [ b(3,16)(rA), b(3,16)(rA), b(3,16)(rA) ])

        IF (SIZE( array ) /= 3)                  CALL zzrc( 50_4 )

        DO i = 1, SIZE( array )
            IF (array( i ) /= bA( i ))           CALL zzrc( (60_4+INT(i, 4)) )
        END DO


        ASSOCIATE(nestedArray => [ array,b(3,16)(rA),b(3,16)(rA),b(3,16)(rA) ])

            IF (SIZE( nestedArray ) /= 6)        CALL zzrc( 70_4 )

            DO i = 1, SIZE( nestedArray )
                j = MOD((i - 1), 3) + 1
                IF (nestedArray( i ) /= bA( j )) CALL zzrc( (80_4+INT(i, 4)) )
            END DO

        END ASSOCIATE

        CALL CheckAssumedLengthParameter( array )

    END ASSOCIATE

    CONTAINS

        SUBROUTINE CheckAssumedLengthParameter( a )
            TYPE(b(*,16)) :: a( : )


            ASSOCIATE(assumedArray => [ a,b(3,16)(rA),b(3,16)(rA),b(3,16)(rA) ])

                IF (SIZE( assumedArray ) /= 6)        CALL zzrc( 90_4 )

                DO i = 1, SIZE( assumedArray )
                    j = MOD((i - 1), 3) + 1
                    IF (assumedArray( i ) /= bA( j ))&
                                            CALL zzrc( (100_4+INT(i, 4)) )
                END DO

            END ASSOCIATE

        END SUBROUTINE CheckAssumedLengthParameter

END PROGRAM aceStrConstrSelector01
