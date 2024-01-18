!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : componentSelector02
!*  TEST CASE TITLE            : selector is a Component of a Derived
!*                               Type
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : September  4, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is the Component of a variable
!*                               of Dervied Type
!*  SECONDARY FUNCTIONS TESTED : The Component is ALLOCATABLE
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Testing where the selector is a Component of a Derived Type that uses
!*  Type Parameters, and the Component:
!*  * Has the ALLOCATABLE Attribute
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE xMod
    IMPLICIT NONE

    TYPE x(l1,k1)
        INTEGER, KIND :: k1
        INTEGER, LEN :: l1

        REAL(k1), ALLOCATABLE :: range( : )

        CONTAINS

            PROCEDURE, PASS :: Set4Range
            PROCEDURE, PASS :: Set8Range
            GENERIC :: SetRange => Set4Range, Set8Range

    END TYPE x

    CONTAINS

        SUBROUTINE Set4Range(this, n)
            CLASS(x(*,4)) :: this
            INTEGER :: n

            INTEGER :: i


            ALLOCATE(this%range( this%l1 ),&
                    SOURCE=[ ((1.0_4 / i), i = n, (this%l1 + n - 1)) ])

        END SUBROUTINE Set4Range

        SUBROUTINE Set8Range(this, n)
            CLASS(x(*,8)) :: this
            INTEGER :: n

            INTEGER :: i


            ALLOCATE(this%range( this%l1 ),&
                    SOURCE=[ ((1.0_8 / i), i = n, (this%l1 + n - 1)) ])

        END SUBROUTINE Set8Range

END MODULE xMod


MODULE yMod
    USE xMod

    IMPLICIT NONE

    TYPE, EXTENDS(x) :: y(k2)
        INTEGER(k1), KIND :: k2

        INTEGER(k2), ALLOCATABLE :: intRange( :,: )

        CONTAINS

            PROCEDURE, PASS :: Set4IntRange
            PROCEDURE, PASS :: Set8IntRange
            GENERIC :: SetIntRange => Set4IntRange, Set8IntRange

    END TYPE y

    CONTAINS

        SUBROUTINE Set4IntRange(this, n)
            CLASS(y(*,4,4)) :: this
            INTEGER :: n

            INTEGER :: i


            CALL this%SetRange( n )

            ALLOCATE( this%intRange( 2,this%l1 ) )

            this%intRange( 2,: ) = 3
            DO i = 1, this%l1
                this%intRange( 1,i ) = INT((this%range( i ) * 1000), 4)
            END DO

        END SUBROUTINE Set4IntRange

        SUBROUTINE Set8IntRange(this, n)
            CLASS(y(*,8,8)) :: this
            INTEGER :: n

            INTEGER :: i


            CALL this%SetRange( n )

            ALLOCATE( this%intRange( 2,this%l1 ) )

            this%intRange( 2,: ) = 3
            DO i = 1, this%l1
                this%intRange( 1,i ) = INT((this%range( i ) * 1000), 8)
            END DO

        END SUBROUTINE Set8IntRange

END MODULE yMod


PROGRAM componentSelector02
    USE yMod

    IMPLICIT NONE

    INTEGER(4) :: i
    INTEGER(4) :: j
    INTEGER :: k

    REAL(8) :: value
    INTEGER(8) :: intValue

    TYPE(x(4,4)) :: quarterRange
    TYPE(y(:,8,8)), ALLOCATABLE :: decimalRange( : )


    CALL quarterRange%SetRange( 1 )
    PRINT *, "quarterRange%range = (", quarterRange%range, ")"

    ASSOCIATE(aRange => quarterRange%range)
        IF (SIZE( aRange ) /= 4) THEN
            CALL zzrc( 250_4 )

        ELSE IF (KIND( aRange ) /= 4) THEN
            CALL zzrc( 251_4 )
        END IF

        DO i = 1_4, 4_4
            IF (aRange( i ) /= (1.0_4 / i)) THEN
                CALL zzrc( (251_4 + i) )
            END IF
        END DO
    END ASSOCIATE


    PRINT *
    PRINT *, "decimalRange:"

    ALLOCATE( y(10,8,8)::decimalRange( 10 ) )

    DO i = 1, 10
        ASSOCIATE(thisRange => decimalRange( i ))
            CALL thisRange%SetIntRange( (i * 10) )
        END ASSOCIATE
    END DO


    DO i = 10, 1, -1
        ASSOCIATE(&
            thisRange => decimalRange( i )%range,&
            thisIntRange => decimalRange( i )%intRange)

            IF (SIZE( thisRange ) /= 10) THEN
                    CALL zzrc( (200_4 + i) )

            ELSE IF (KIND( thisRange ) /= 8) THEN
                    CALL zzrc( (210_4 + i) )

            ELSE IF ( ANY(SHAPE( thisIntRange ) /= [ 2,10 ]) ) THEN
                    CALL zzrc( (220_4 + i) )

            ELSE IF (KIND( thisIntRange ) /= 8) THEN
                    CALL zzrc( (230_4 + i) )

            ELSE IF ( ANY(thisIntRange( 2,: ) /= 3) ) THEN
                    CALL zzrc( (240_4 + i) )
            END IF


            PRINT *

            DO j = 1, 10
                k = (i * 10) + j - 1
                value = 1.0_8 / k
                intValue = INT((value * 1000), 8)
                PRINT *, i, j, k, value, thisRange( j ),&
                                    intValue, thisIntRange( 1,j )

                IF (thisRange( j ) /= value) THEN
                    CALL zzrc( j )

                ELSE IF (thisIntRange( 1,j ) /= intValue) THEN
                    CALL zzrc( (100_4 + j) )
                END IF
            END DO
        END ASSOCIATE
    END DO

END PROGRAM componentSelector02
