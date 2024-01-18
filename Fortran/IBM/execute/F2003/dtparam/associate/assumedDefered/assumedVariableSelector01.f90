!***********************************************************************
!* =====================================================================
!*
!*                               Parameter(s)
!*
!*  DATE                       : August 15, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is a variable
!*  SECONDARY FUNCTIONS TESTED : The Variable has an Assumed Length Parameter
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Using the Basic Testing (as defined above):
!*  o selector has Assumed Length Type Parameters
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mod
    IMPLICIT NONE

    TYPE base(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        REAL(k1) :: vector( l1 )
    END TYPE base

    TYPE, EXTENDS(base) :: extended(l2)
        INTEGER, LEN :: l2
        INTEGER(k1) :: viktor( l1,l2 )
    END TYPE extended


    REAL(4), POINTER :: vectorVerify( : )
    INTEGER(4), POINTER :: viktorVerify( :,: )


    CONTAINS

        SUBROUTINE VerifyBase(b, rc)
            TYPE(base(*,4)) :: b
            INTEGER(4) :: rc

            PRINT *, "VerifyBase(", rc, ") [", b, "]"

            ASSOCIATE(bAssociateName => b)

                IF (KIND( bAssociateName%vector ) /= KIND( vectorVerify )) THEN
                    CALL zzrc( rc )

                ELSE IF (bAssociateName%k1 /= KIND( vectorVerify )) THEN
                    CALL zzrc( (rc + 1_4) )

                ELSE IF (SIZE( bAssociateName%vector )&
                            /= SIZE( vectorVerify )) THEN
                    CALL zzrc( (rc + 2_4) )

                ELSE IF (bAssociateName%l1 /= SIZE( vectorVerify )) THEN
                    CALL zzrc( (rc + 3_4) )

                ELSE IF ( ANY(bAssociateName%vector /= vectorVerify) ) THEN
                    CALL zzrc( (rc + 4_4) )
                END IF

            END ASSOCIATE

        END SUBROUTINE VerifyBase

        SUBROUTINE VerifyExtented(e, rc)
            TYPE(extended(*,4,*)) :: e
            INTEGER(4) :: rc

            PRINT *, "VerifyExtented(", rc, ") [", e, "]"

            ASSOCIATE(eAssociateName => e)

                IF (KIND( eAssociateName%viktor ) /= KIND( viktorVerify )) THEN
                    CALL zzrc( rc )

                ELSE IF (eAssociateName%k1 /= KIND( viktorVerify )) THEN
                    CALL zzrc( (rc + 1_4) )

                ELSE IF ( ANY(SHAPE( eAssociateName%viktor )&
                                    /= SHAPE( viktorVerify )) ) THEN
                    CALL zzrc( (rc + 2_4) )

                ELSE IF (eAssociateName%l1 /= SIZE(viktorVerify, 1)) THEN
                    CALL zzrc( (rc + 3_4) )

                ELSE IF (eAssociateName%l2 /= SIZE(viktorVerify, 2)) THEN
                    CALL zzrc( (rc + 4_4) )

                ELSE IF ( ANY(eAssociateName%viktor /= viktorVerify) ) THEN
                    CALL zzrc( (rc + 5_4) )
                END IF

                CALL VerifyBase(eAssociateName%base, (rc + 5_4))

            END ASSOCIATE

        END SUBROUTINE VerifyExtented

END MODULE mod


PROGRAM assumedVariableSelector01
    USE mod

    IMPLICIT NONE


    INTERFACE
        SUBROUTINE CheckBase(b, rc)
            USE mod
            IMPLICIT NONE

            TYPE(base(*,4)) :: b( : )
            INTEGER(4) :: rc
        END SUBROUTINE CheckBase
    END INTERFACE



    TYPE(base(1,4)) :: b( 3 )
    TYPE(extended(1,4,3)) :: e


    INTEGER(4), TARGET :: extendedArray( 1,3 ) =&
                RESHAPE([ 99_4,98_4,97_4 ],[ 1,3 ])

    REAL(4), TARGET :: baseArray( 1,4 )

    COMMON /VerifyBlock/ baseArray


    baseArray = RESHAPE([ 1.0_4, 2.0_4, 3.0_4, 4.0_4 ],[ 1,4 ])

    b = [   base(1,4)(baseArray( :,1 )),&
            base(1,4)(baseArray( :,2 )),&
            base(1,4)(baseArray( :,3 )) ]


    CALL CheckBase(b, 10_4)

    ASSOCIATE(bAssociateName => b)

        CALL CheckBase(bAssociateName, 70_4)

    END ASSOCIATE


    e = extended(1,4,3)(baseArray( :,4 ),extendedArray)

    vectorVerify => baseArray( :,4 )
    viktorVerify => extendedArray


    CALL CheckExtended(e, 130_4)

    CALL VerifyExtented(e, 170_4)
    PRINT *

    CALL VerifyBase(e%base, 180_4)
    PRINT *


    ASSOCIATE(eAssociateName => e)

        CALL CheckExtended(eAssociateName, 190_4)

        CALL VerifyExtented(eAssociateName, 230_4)
        PRINT *

        CALL VerifyBase(eAssociateName%base, 240_4)

    END ASSOCIATE


    CONTAINS

        SUBROUTINE CheckExtended(e, rc)
            TYPE(extended(*,4,*)) :: e
            INTEGER(4) :: rc

            CALL VerifyExtented(e, rc)
            PRINT *

            CALL VerifyBase(e%base, (rc + 10_4))
            PRINT *

            ASSOCIATE(eAssociateName => e)

                CALL VerifyExtented(eAssociateName, (rc + 20_4))
                PRINT *

                CALL VerifyBase(eAssociateName%base, (rc + 30_4))
                PRINT *

            END ASSOCIATE

        END SUBROUTINE CheckExtended

END PROGRAM assumedVariableSelector01


SUBROUTINE CheckBase(b, rc)
    USE mod

    IMPLICIT NONE

    TYPE(base(*,4)) :: b( : )
    INTEGER(4) :: rc

    INTEGER(4) :: i

    REAL(4), TARGET :: baseArray( 1,4 )

    COMMON /VerifyBlock/ baseArray


    DO i = 1_4, SIZE( b )
        vectorVerify => baseArray( :,i )
        CALL VerifyBase(b( i ), (rc + ((i - 1_4) * 10_4)))
    END DO

    PRINT *

    ASSOCIATE(bAssociateName => b)

        DO i = 1_4, SIZE( bAssociateName )
            vectorVerify => baseArray( :,i )
            CALL VerifyBase(bAssociateName( i ), (rc + ((i + 2_4) * 10_4)))
        END DO

        PRINT *

    END ASSOCIATE

END SUBROUTINE CheckBase
