!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : July  9, 2008
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
!*     -  Single primary (an Array Constructor -- using an Array
!*        within the Array Constructor for a Derived Type with a
!*        LEN Parameter)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

PROGRAM exprSelector02b
    IMPLICIT NONE

    TYPE tL(l)
        INTEGER, LEN :: l

        CHARACTER(l) :: str
    END TYPE tL


    INTEGER(4) :: i
    INTEGER(4) :: j

    CHARACTER(5), PARAMETER :: strArray( 4 ) =&
                    [ 'Hello', 'to   ', 'TesT ', 'XLF  ' ]

    INTEGER, ALLOCATABLE :: valuesArray( : )
    INTEGER, ALLOCATABLE :: checkKindArray( : )
    INTEGER, ALLOCATABLE :: checkValuesArray( : )

    TYPE(tL(4)) :: tLArray( 4 ) =&
        [   tL(4)(strArray( 1 )), tL(4)(strArray( 2 )),&
            tL(4)(strArray( 3 )), tL(4)(strArray( 4 ))  ]


    ASSOCIATE(array => [ tL(3):: ])
        IF (SIZE( array ) /= 0) THEN
            CALL zzrc( 10_4 )

        ELSE IF (array%l /= 3) THEN
            CALL zzrc( 11_4 )
        END IF
    END ASSOCIATE


    ALLOCATE(valuesArray( 4 ), SOURCE=[ 3, 5, 2, 4 ])
    ASSOCIATE(array => [ tLArray ])
        j = SIZE( array )

        ALLOCATE( checkValuesArray( j ) )

        DO i = 1_4, j
            checkValuesArray( i ) = LEN( array( i )%str )

            IF (array( i )%str /= tlArray( i )%str) THEN
                CALL zzrc( (20_4 + i) )

            ELSE IF (array( i )%str /= strArray( i )( :4 )) THEN
                CALL zzrc( (30_4 + i) )
            END IF
        END DO

        IF (SIZE( array ) /= 4) THEN
            CALL zzrc( 40_4 )

        ELSE IF ( ANY(checkValuesArray /= 4) ) THEN
            CALL zzrc( 41_4 )
        END IF

        DEALLOCATE( checkValuesArray )
    END ASSOCIATE

    DEALLOCATE( valuesArray )

END PROGRAM exprSelector02b
