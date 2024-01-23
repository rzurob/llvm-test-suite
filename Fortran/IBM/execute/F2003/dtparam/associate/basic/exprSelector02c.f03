!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : July 14, 2008
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
!*     -  Single primary (an Array Constructor -- using variables
!*        within the Array Constructor for a Derived Type with both
!*        KIND/LEN Parameters)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

PROGRAM exprSelector02c
    IMPLICIT NONE


    TYPE tK(k)
        INTEGER, KIND :: k

        INTEGER(k) :: len
    END TYPE tK

    TYPE tL(l)
        INTEGER, LEN :: l

        CHARACTER(l) :: str
    END TYPE tL


    TYPE tLK(l,k)
        INTEGER, LEN :: l
        INTEGER, KIND :: k

        TYPE(tK(k)) :: len
        TYPE(tL(l)) :: str
    END TYPE tLK


    INTEGER(4) :: i

    CHARACTER(5), PARAMETER :: strArray( 5 )&
            = [ 'to   ', 'tESt ', 'thE  ', 'IBM  ', 'World' ]


    TYPE(tLK(5,8)) :: tLK_a =&
        tLK(5,8)(tK(8)(LEN_TRIM(strArray( 1 ))),tL(5)(strArray( 1 )))
    TYPE(tLK(5,8)) :: tLK_b =&
        tLK(5,8)(tK(8)(LEN_TRIM(strArray( 2 ))),tL(5)(strArray( 2 )))
    TYPE(tLK(5,8)) :: tLK_c =&
        tLK(5,8)(tK(8)(LEN_TRIM(strArray( 3 ))),tL(5)(strArray( 3 )))
    TYPE(tLK(5,8)) :: tLK_d =&
        tLK(5,8)(tK(8)(LEN_TRIM(strArray( 4 ))),tL(5)(strArray( 4 )))
    TYPE(tLK(5,8)) :: tLK_e =&
        tLK(5,8)(tK(8)(LEN_TRIM(strArray( 5 ))),tL(5)(strArray( 5 )))


    ASSOCIATE(array => [ tLK(13,8):: ])
        IF (SIZE( array ) /= 0) THEN
            ERROR STOP 10_4

        ELSE IF (array%l /= 13) THEN
            ERROR STOP 11_4

        ELSE IF (array%k /= 8) THEN
            ERROR STOP 12_4
        END IF
    END ASSOCIATE


    ASSOCIATE(array => [ tLK_a, tLK_b, tLK_c, tLK_d, tLK_e ])
        IF (SIZE( array ) /= 5) THEN
            ERROR STOP 20_4
        END IF

        DO i = 1_4, SIZE( array )
            IF (KIND( array( i )%len%len ) /= 8) THEN
                CALL zzrc( (30_4 + i) )

            ELSE IF (array( i )%len%len /= LEN_TRIM( strArray( i ) )) THEN
                CALL zzrc( (40_4 + i) )

            ELSE IF (LEN( array( i )%str%str ) /= 5) THEN
                CALL zzrc( (50_4 + i) )

            ELSE IF (LEN_TRIM( array( i )%str%str ) /= array( i )%len%len) THEN
                CALL zzrc( (60_4 + i) )

            ELSE IF (array( i )%str%str /= strArray( i )) THEN
                CALL zzrc( (70_4 + i) )
            END IF
        END DO
    END ASSOCIATE

END PROGRAM exprSelector02c
