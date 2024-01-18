!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : arraySection01
!*  TEST CASE TITLE            : selector is an Array Section
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : August 27, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : The ASSOCIATE Construct
!*  SECONDARY FUNCTIONS TESTED : selector is an Array Section
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Miscellaneous Testing:
!*  o Where selector is an:
!*    * Array Section
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE tBMod
    IMPLICIT NONE

    TYPE tB(k,l)
        INTEGER, KIND :: k
        INTEGER, LEN :: l

        LOGICAL(k) :: bArray( l,l )
    END TYPE tB

END MODULE tBMod


MODULE tEMod
    USE tBMod

    IMPLICIT NONE

    TYPE, EXTENDS(tB) :: tE(l2)
        INTEGER, LEN :: l2

        TYPE(tB(k,l)) :: eArray( l2 )
    END TYPE tE

END MODULE tEMod


PROGRAM arraySection01
    USE tEMod

    IMPLICIT NONE

    INTEGER(4) :: i
    INTEGER(4) :: j
    INTEGER(4) :: k
    INTEGER(4) :: l

    TYPE(tB(2,3)) :: base( 4 )
    TYPE(tE(2,3,4)) :: extended( 3 )


    ! base( 1 )%bArray( :,: ) = F F F  F F F  F F F
    ! base( 2 )%bArray( :,: ) = T ... T
    ! base( 3 )%bArray( :,: ) = F ... F
    ! base( 4 )%bArray( :,: ) = T ... T
    base = [ (tB(2,3)(&
            RESHAPE([ ((MOD(((i * 10) + j), 2) == 0), i = 1, 9) ], [ 3,3 ])),&
                j = 1, 4) ]

    DO j = 1_4, 3_4
        k = (j - 1_4) * 20_4

        ASSOCIATE(bName => base( j:(j + 1_4) ))

            DO i = 1_4, 2_4
                l = (i * 10_4) + k

                IF (SIZE( bName ) /= 2) THEN
                    CALL zzrc( l )


                ELSE IF (KIND( bName( i )%bArray ) /= 2) THEN
                    CALL zzrc( (l + 1_4) )

                ELSE IF (bName( i )%k /= 2) THEN
                    CALL zzrc( (l + 2_4) )


                ELSE IF ( ANY(SHAPE( bName( i )%bArray ) /= [ 3,3 ]) ) THEN
                    CALL zzrc( (l + 3_4) )

                ELSE IF (bName( i )%l /= 3) THEN
                    CALL zzrc( (l + 4_4) )


                ELSE IF ( ANY(bName( i )%bArray .NEQV.&
                            (MOD((i + j), 2) == 1)) ) THEN
                    CALL zzrc( (l + 5_4) )

                ELSE IF ( ANY(bName( i )%bArray .NEQV.&
                                base( (j + i - 1_4) )%bArray) ) THEN
                    CALL zzrc( (l + 6_4) )
                END IF
            END DO

        END ASSOCIATE
    END DO


    extended = [ (tE(2,3,4)(base( i )%bArray,base), i = 1, 3) ]

    DO j = 1_4, 3_4
        k = j * 20_4

        ASSOCIATE(eName => extended( j:j ))

            IF (SIZE( eName ) /= 1) THEN
                CALL zzrc( k )


            ELSE IF (eName%k /= 2) THEN
                CALL zzrc( (k + 1_4) )

            ELSE IF (eName( 1 )%eArray%k /= 2) THEN
                CALL zzrc( (k + 2_4) )

            ELSE IF (KIND( eName( 1 )%bArray ) /= 2) THEN
                CALL zzrc( (k + 3_4) )


            ELSE IF (eName%l /= 3) THEN
                CALL zzrc( (k + 4_4) )

            ELSE IF (eName( 1 )%eArray%l /= 3) THEN
                CALL zzrc( (k + 5_4) )


            ELSE IF (SIZE( eName( 1 )%eArray ) /= 4) THEN
                CALL zzrc( (k + 6_4) )

            ELSE IF ( ANY(eName( 1 )%bArray .NEQV. (MOD(j, 2) /= 1)) ) THEN
                CALL zzrc( (k + 7_4) )
            END IF

            DO i = 1_4, 4_4
                k = (j * 20_4) + 10_4 + ((i - 1_4) * 2_4)

                ASSOCIATE(bA => eName( 1 )%eArray( i )%bArray)
                    IF ( ANY(SHAPE( bA ) /= [ 3,3 ]) ) THEN
                        CALL zzrc( k )

                    ELSE IF ( ANY(bA .NEQV. base( i )%bArray) ) THEN
                        CALL zzrc( (k + 1_4) )
                    END IF
                END ASSOCIATE
            END DO

        END ASSOCIATE
    END DO

END PROGRAM arraySection01
