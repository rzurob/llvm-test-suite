!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : exprSelector03
!*  TEST CASE TITLE            : expr selector with Derived Type Parameters
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : July 14, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is an Expression
!*  SECONDARY FUNCTIONS TESTED : The Expression is an Intrinsic Function
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Basic Testing where selector is:
!*  o  An expr that contains a:
!*     -  Single primary (the Result from an Intrinsic FUNCTION)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

PROGRAM exprSelector03
    IMPLICIT NONE


    TYPE b(l)
        INTEGER, LEN :: l
        COMPLEX(4) :: array( l )
    END TYPE b

    TYPE, EXTENDS(b) :: d(k)
        INTEGER, KIND :: k
        INTEGER(k) :: count
    END TYPE d

    INTEGER :: i
    INTEGER :: j

    TYPE(b(3)) :: base
    TYPE(b(1)) :: checkBase( 3 )

    TYPE(d(1,4)) :: derived( 2,2 )
    TYPE(d(1,4)) :: checkTranspose( 2,2 )

    INTEGER :: shapeTranspose( 2 )


    base = b(3)([ (3,3), (2,2), (1,1) ])

    checkBase = [ b(1)( [ (3,3) ] ), b(1)( [ (2,2) ] ), b(1)( [ (1,1) ] ) ]

    ASSOCIATE(transfer => TRANSFER(base, checkBase))
        IF (transfer%l /= 1) THEN
            STOP 10

        ELSE IF (SIZE( transfer ) /= 3) THEN
            STOP 11

        ELSE IF ( ANY(transfer%array( 1 ) /= base%array) ) THEN
            STOP 12
        END IF
    END ASSOCIATE


    derived = RESHAPE([ d(1,4)([ (1,2) ], 3), d(1,4)([ (3,4) ], 7),&
                        d(1,4)([ (5,6) ],11), d(1,4)([ (7,8) ],15) ], [ 2,2 ])

    shapeTranspose = [ 2,2 ]

    checkTranspose =&
        RESHAPE([ d(1,4)([ (1,2) ], 3), d(1,4)([ (5,6) ],11),&
                  d(1,4)([ (3,4) ], 7), d(1,4)([ (7,8) ],15) ], [ 2,2 ])

    ASSOCIATE(transpose => TRANSPOSE( derived ))
        IF (transpose%l /= 1) THEN
            STOP 20

        ELSE IF (transpose%k /= 4) THEN
            STOP 21

        ELSE IF ( ANY(SHAPE( transpose ) /= shapeTranspose) ) THEN
            STOP 22

        ELSE IF ( ANY(transpose%array( 1 ) /= checkTranspose%array( 1 )) ) THEN
            STOP 23

        ELSE IF ( ANY(transpose%count /= checkTranspose%count ) ) THEN
            STOP 24
        END IF
    END ASSOCIATE

END PROGRAM exprSelector03
