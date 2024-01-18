!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : exprSelector02a
!*
!*  DATE                       : July  9, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is an Expression
!*  SECONDARY FUNCTIONS TESTED : The Expression is an Array Constructor
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
!*     -  Single primary (an Array Constructor -- using Structure
!*        Constructors within the Array Constructor for a Derived
!*        Type with a KIND Parameter)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

PROGRAM exprSelector02a
    IMPLICIT NONE

    TYPE tK(k)
        INTEGER, KIND :: k

        CHARACTER(:,k), ALLOCATABLE :: str
    END TYPE tK


    INTEGER(4) :: i
    INTEGER(4) :: j

    CHARACTER(5) :: strArray( 4 ) = [ 'IBM  ', 'World', 'to   ', 'tESt ' ]

    INTEGER, ALLOCATABLE :: valuesArray( : )
    INTEGER, ALLOCATABLE :: checkKindArray( : )
    INTEGER, ALLOCATABLE :: checkValuesArray( : )


    TYPE(tK(1)) :: tK_1


    tK_1 = tK(1)('to')


    ASSOCIATE(array => [ tK(1):: ])
        IF (SIZE( array ) /= 0) THEN
            CALL zzrc( 10_4 )

        ELSE IF (array%k /= 1) THEN
            CALL zzrc( 11_4 )
        END IF
    END ASSOCIATE


    ALLOCATE(valuesArray( 4 ), SOURCE=[ 3, 5, 2, 4 ])
    ASSOCIATE(array => [ tK(1)('IBM'), tK(1)('World'), tK_1, tK(1)('tESt') ])

        j = SIZE( array )

        ALLOCATE( checkKindArray( j ) )
        ALLOCATE( checkValuesArray( j ) )

        DO i = 1_4, j
            checkKindArray( i ) = KIND( array( i )%str )
            checkValuesArray( i ) = LEN( array( i )%str )

            IF (array( i )%str /= TRIM( strArray( i ) )) THEN
                CALL zzrc( (20_4 + i) )
            END IF
        END DO

        IF (SIZE( array ) /= 4) THEN
            CALL zzrc( 30_4 )

        ELSE IF ( ANY(checkKindArray /= 1) ) THEN
            CALL zzrc( 31_4 )

        ELSE IF ( ANY(checkValuesArray /= valuesArray) ) THEN
            CALL zzrc( 32_4 )
        END IF

        DEALLOCATE( checkKindArray )
        DEALLOCATE( checkValuesArray )
    END ASSOCIATE

    DEALLOCATE( valuesArray )

END PROGRAM exprSelector02a
