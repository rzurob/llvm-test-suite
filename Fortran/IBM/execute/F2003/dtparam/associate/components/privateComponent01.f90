!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : privateComponent01
!*  TEST CASE TITLE            : variable selector with a PRIVATE
!*                               Component
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : August 21, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is a variable
!*  SECONDARY FUNCTIONS TESTED : The Variable has a Component with the
!*                               PRIVATE Attribute
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Using the Basic Testing (as defined above), where selector:
!*  * Is a Derived Type with:
!*    o A PRIVATE Component
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE ptMod
    IMPLICIT NONE

    TYPE point(k,l)
        INTEGER(8), KIND :: k
        INTEGER(8), LEN :: l

        PRIVATE
            REAL(k) :: coord( l )
    END TYPE point


    CONTAINS

        SUBROUTINE SetCoord(pt, coord)
            CLASS(*) :: pt
            CLASS(*), TARGET :: coord( : )

            REAL(4), POINTER :: c4( : )
            REAL(8), POINTER :: c8( : )
            REAL(16), POINTER :: c16( : )

            SELECT TYPE ( coord )
                TYPE IS (REAL(4))
                    c4 => coord
                TYPE IS (REAL(8))
                    c8 => coord
                TYPE IS (REAL(16))
                    c16 => coord
            END SELECT

            SELECT TYPE ( pt )
                TYPE IS (point(4,*))
                    pt%coord = c4
                TYPE IS (point(8,*))
                    pt%coord = c8
                TYPE IS (point(16,*))
                    pt%coord = c16
            END SELECT

        END SUBROUTINE

        SUBROUTINE CheckPoint(pt, rc)
            CLASS(*) :: pt
            INTEGER(4) :: rc

            CHARACTER(111) :: format

            SELECT TYPE ( pt )
                TYPE IS (point(4,*))
                    WRITE(format, 10) SIZE( pt%coord )
                    WRITE(6,format) pt%k, KIND( pt%coord ),&
                                    pt%l, SIZE( pt%coord ), pt%coord

                TYPE IS (point(8,*))
                    WRITE(format, 10) SIZE( pt%coord )
                    WRITE(6,format) pt%k, KIND( pt%coord ),&
                                    pt%l, SIZE( pt%coord ), pt%coord

                TYPE IS (point(16,*))
                    WRITE(format, 10) SIZE( pt%coord )
                    WRITE(6,format) pt%k, KIND( pt%coord ),&
                                    pt%l, SIZE( pt%coord ), pt%coord

                CLASS DEFAULT
                    CALL zzrc( rc )
            END SELECT

10          FORMAT('("pt%k = ",I2,", KIND(pt%coord) = ",I2,", pt%l = ",I1,", SIZE(pt%coord) = ",I1,", pt%coord = ",',I1,'(F3.1," "))')

        END SUBROUTINE CheckPoint

END MODULE ptMod


PROGRAM privateComponent01
    USE ptMod

    IMPLICIT NONE

    REAL(4)  :: pt4_1( 1 )  = [ 1.0_4 ]
    REAL(8)  :: pt8_1( 1 )  = [ 1.0_8 ]
    REAL(16) :: pt16_1( 1 ) = [ 1.0_16 ]

    REAL(4)  :: pt4_2( 2 )  = [ 2.0_4,  2.0_4 ]
    REAL(8)  :: pt8_2( 2 )  = [ 2.0_8,  2.0_8 ]
    REAL(16) :: pt16_2( 2 ) = [ 2.0_16, 2.0_16 ]

    REAL(4)  :: pt4_3( 3 )  = [ 3.0_4,  3.0_4,  3.0_4 ]
    REAL(8)  :: pt8_3( 3 )  = [ 3.0_8,  3.0_8,  3.0_8 ]
    REAL(16) :: pt16_3( 3 ) = [ 3.0_16, 3.0_16, 3.0_16 ]

    REAL(4)  :: pt4_4( 4 )  = [ 4.0_4,  4.0_4,  4.0_4,  4.0_4 ]
    REAL(8)  :: pt8_4( 4 )  = [ 4.0_8,  4.0_8,  4.0_8,  4.0_8 ]
    REAL(16) :: pt16_4( 4 ) = [ 4.0_16, 4.0_16, 4.0_16, 4.0_16 ]


    TYPE(point(4,1)) :: point4_1
    TYPE(point(4,2)) :: point4_2
    TYPE(point(4,3)) :: point4_3
    TYPE(point(4,4)) :: point4_4

    TYPE(point(8,1)) :: point8_1
    TYPE(point(8,2)) :: point8_2
    TYPE(point(8,3)) :: point8_3
    TYPE(point(8,4)) :: point8_4

    TYPE(point(16,1)) :: point16_1
    TYPE(point(16,2)) :: point16_2
    TYPE(point(16,3)) :: point16_3
    TYPE(point(16,4)) :: point16_4

    !
    !  Kind 4
    !
    CALL SetCoord(point4_1, pt4_1)
    ASSOCIATE(pt => point4_1)
        CALL CheckPoint(pt, 10_4)
    END ASSOCIATE

    CALL SetCoord(point4_2, pt4_2)
    ASSOCIATE(pt => point4_2)
        CALL CheckPoint(pt, 20_4)
    END ASSOCIATE

    CALL SetCoord(point4_3, pt4_3)
    ASSOCIATE(pt => point4_3)
        CALL CheckPoint(pt, 30_4)
    END ASSOCIATE

    CALL SetCoord(point4_4, pt4_4)
    ASSOCIATE(pt => point4_4)
        CALL CheckPoint(pt, 40_4)
    END ASSOCIATE

    PRINT *


    !
    !  Kind 8
    !
    CALL SetCoord(point8_1, pt8_1)
    ASSOCIATE(pt => point8_1)
        CALL CheckPoint(pt, 50_4)
    END ASSOCIATE

    CALL SetCoord(point8_2, pt8_2)
    ASSOCIATE(pt => point8_2)
        CALL CheckPoint(pt, 60_4)
    END ASSOCIATE

    CALL SetCoord(point8_3, pt8_3)
    ASSOCIATE(pt => point8_3)
        CALL CheckPoint(pt, 70_4)
    END ASSOCIATE

    CALL SetCoord(point8_4, pt8_4)
    ASSOCIATE(pt => point8_4)
        CALL CheckPoint(pt, 80_4)
    END ASSOCIATE

    PRINT *


    !
    !  Kind 16
    !
    CALL SetCoord(point16_1, pt16_1)
    ASSOCIATE(pt => point16_1)
        CALL CheckPoint(pt, 90_4)
    END ASSOCIATE

    CALL SetCoord(point16_2, pt16_2)
    ASSOCIATE(pt => point16_2)
        CALL CheckPoint(pt, 100_4)
    END ASSOCIATE

    CALL SetCoord(point16_3, pt16_3)
    ASSOCIATE(pt => point16_3)
        CALL CheckPoint(pt, 110_4)
    END ASSOCIATE

    CALL SetCoord(point16_4, pt16_4)
    ASSOCIATE(pt => point16_4)
        CALL CheckPoint(pt, 120_4)
    END ASSOCIATE

END PROGRAM privateComponent01
