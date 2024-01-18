!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : exprSelector04
!*
!*  DATE                       : July 14, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is an Expression
!*  SECONDARY FUNCTIONS TESTED : The Expression is a User Defined Function
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Basic Testing where selector is:
!*  o  An expr that contains a:
!*     -  Single primary (the Result from a User Defined FUNCTION)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mod

    TYPE bkT(k)
        INTEGER, KIND :: k

        INTEGER(k) :: kind = k
    END TYPE bkT

    TYPE blT(l)
        INTEGER, LEN :: l

        INTEGER :: len( l )
    END TYPE blT

    TYPE, EXTENDS(blT) :: blTk(k)
        INTEGER, KIND :: k

        TYPE(bkT(k)) :: kind
    END TYPE blTk

    CONTAINS

        TYPE(bkT(8)) FUNCTION mFunc(array, n)
            TYPE(bkT(8)) :: array( * )
            INTEGER :: n


            mFunc = array( n )

        END FUNCTION mFunc

END MODULE mod


PROGRAM exprSelector04
    USE mod

    IMPLICIT NONE

    INTERFACE
        FUNCTION eFunc(array, l, m, n)
            USE mod

            TYPE(blTk(3,4)) :: array( * )
            INTEGER :: l
            INTEGER :: m
            INTEGER :: n

            TYPE(blTk(3,4)) :: eFunc( 3 )
        END FUNCTION eFunc
    END INTERFACE

    INTEGER :: i
    INTEGER :: j

    INTEGER, PARAMETER :: one( 1 ) = [ 1 ]
    INTEGER, PARAMETER :: two( 1 ) = [ 2 ]
    INTEGER, PARAMETER :: three( 1 ) = [ 3 ]

    INTEGER :: check( 3 )
    INTEGER :: blTkCheck( 9 ) = [ 16, 17, 18, 10, 11, 12, 4, 5, 6 ]

    TYPE(bkT(8)) :: bkTArray( 5 ) = [ (bkT(8)(i), i = 1, 5) ]

    TYPE(blT(1)) :: blTArray( 3 ) =&
        [ blT(1)( three ), blT(1)( two ), blT(1)( one ) ]

    TYPE(blTK(3,4)) :: blTKArray( 7 ) =&
        [blTk(3,4)([ 1,  2,  3],bkT(4)(4)), blTk(3,4)([ 4,  5,  6],bkT(4)(4)),&
         blTk(3,4)([ 7,  8,  9],bkT(4)(4)), blTk(3,4)([10, 11, 12],bkT(4)(4)),&
         blTk(3,4)([13, 14, 15],bkT(4)(4)), blTk(3,4)([16, 17, 18],bkT(4)(4)),&
         blTk(3,4)([19, 20, 21],bkT(4)(4))]


    ASSOCIATE(scalar => mFunc(bkTArray, 3))
        IF (scalar%k /= 8) THEN
            STOP 10

        ELSE IF (scalar%kind /= 3) THEN
            STOP 11
        END IF
    END ASSOCIATE


    ASSOCIATE(range => pFunc(blTArray, 1, 3))
        IF (SIZE( range ) /= 3) THEN
            STOP 20
        END IF

!       ac-implied-do had issues, ... thus it was replaced
!        check = [ (SIZE( range( i )%len ), i = 1, SIZE( range )) ]
        check = [ SIZE( range( 1 )%len ),&
                  SIZE( range( 2 )%len ), SIZE( range( 3 )%len ) ]
        IF ( ANY(check /= 1) ) THEN
            STOP 21
        END IF

        check = [ (range( i )%len, i = 1, SIZE( range )) ]
        IF ( ANY(check /= [ three, two, one ]) ) THEN
            STOP 22
        END IF
    END ASSOCIATE


    ASSOCIATE(vector => eFunc(blTKArray, 6, 4, 2))
        IF (SIZE( vector ) /= 3) THEN
            STOP 30
        END IF

        check = [ (SIZE( vector( i )%len ), i = 1, SIZE( vector )) ]
        IF ( ANY(check /= 3) ) THEN
            STOP 31

        ELSE IF ( ANY(vector%kind%kind /= 4) ) THEN
            STOP 32
        END IF

        DO i = 6, 2, -2
            j = 4 - (i / 2)
            check = vector( j )%len
            IF ( ANY(check /= blTKArray( i )%len) ) THEN
                STOP 33
            END IF
        END DO
    END ASSOCIATE


    CONTAINS

        FUNCTION pFunc(array, m, n)
            TYPE(blT(1)) :: array( * )
            INTEGER :: m
            INTEGER :: n

            TYPE(blT(1)) :: pFunc( (n - m + 1) )


            pFunc = array( m:n )

        END FUNCTION pFunc

END PROGRAM exprSelector04


FUNCTION eFunc(array, l, m, n)
    USE mod

    TYPE(blTk(3,4)) :: array( * )
    INTEGER :: l
    INTEGER :: m
    INTEGER :: n

    TYPE(blTk(3,4)) :: eFunc( 3 )


    eFunc( 1 ) = array( l )
    eFunc( 2 ) = array( m )
    eFunc( 3 ) = array( n )

END FUNCTION eFunc
