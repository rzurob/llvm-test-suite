!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : arraySection02
!*
!*  DATE                       : September 16, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : The ASSOCIATE Construct
!*  SECONDARY FUNCTIONS TESTED : selector is an Array Section (with a
!*                               Vector Subscript)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Miscellaneous Testing:
!*  o Where selector is an:
!*    * Array Section (with a Vector Subscript)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mod
    IMPLICIT NONE

    TYPE tBase(l,k)
        INTEGER, LEN :: l
        INTEGER, KIND :: k

        INTEGER(k) :: field
        INTEGER(k) :: array( l )
    END TYPE tBase

END MODULE mod


PROGRAM arraySection02
    USE mod

    IMPLICIT NONE

    INTEGER(4) :: i
    INTEGER(4) :: j

    INTEGER :: vector( 2 ) = [ 1, 3 ]

    INTEGER(4) :: baseArray( 3,3 )

    TYPE(tBase(3,4)), TARGET :: base( 3 )


    baseArray = RESHAPE([ (i, i = 1, 9) ], [ 3,3 ])
    base = [ (tBase(3,4)(i,baseArray( :,i )), i = 1, 3) ]

    ASSOCIATE(b => base( vector ))

        IF (SIZE( b ) /= SIZE( vector )) THEN
            CALL zzrc( 5_4 )
        END IF

        DO i = 1, 2
            j = i * 10_4

            IF (b( i )%field /= vector( i )) THEN
                CALL zzrc( (j + 1_4) )

            ELSE IF ( ANY(b( i )%array /= baseArray( :,vector( i ) )) ) THEN
                CALL zzrc( (j + 2_4) )
            END IF


            ASSOCIATE(bA => b( i )%array( vector ))

                IF (SIZE( bA ) /= SIZE( vector )) THEN
                    CALL zzrc( (j + 3_4) )

                ELSE IF ( ANY(bA /= b( i )%array( vector )) ) THEN
                    CALL zzrc( (j + 4_4) )

                ELSE IF ( ANY(bA /= baseArray( vector,vector( i ) )) ) THEN
                    CALL zzrc( (j + 5_4) )
                END IF

            END ASSOCIATE
        END DO

    END ASSOCIATE


    DO i = 1, 3
        j = (i * 10_4) + 100_4

        ASSOCIATE(b => base( i )%array( vector ))
            IF (SIZE( b ) /= SIZE( vector )) THEN
                CALL zzrc( (j + 1_4) )

            ELSE IF ( ANY(b /= base( i )%array( vector )) ) THEN
                CALL zzrc( (j + 2_4) )

            ELSE IF ( ANY(b /= baseArray( vector,i )) ) THEN
                CALL zzrc( (j + 3_4) )
            END IF
        END ASSOCIATE
    END DO

END PROGRAM arraySection02
