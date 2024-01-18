!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : pointerComponent01
!*
!*  DATE                       : August 20, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is a variable
!*  SECONDARY FUNCTIONS TESTED : The Variable has a Component with the
!*                               POINTER Attribute
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Using the Basic Testing (as defined above), where selector:
!*  * Is a Derived Type with:
!*    o A POINTER Component
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE cMod
    IMPLICIT NONE

    TYPE contained(l,k)
        INTEGER, LEN :: l
        INTEGER, KIND :: k

        INTEGER(k) :: value( l )
    END TYPE contained

END MODULE cMod


MODULE bMod
    USE cMod

    IMPLICIT NONE

    TYPE base(k,l)
        INTEGER, KIND :: k
        INTEGER, LEN :: l

        TYPE(contained(l,k)), POINTER :: cBasePtr
    END TYPE base

END MODULE bMod


MODULE dMod
    USE bMod

    IMPLICIT NONE

    TYPE, EXTENDS(base) :: derived
        TYPE(contained(l,k)), POINTER :: cDerivedPtr
    END TYPE derived

END MODULE dMod


PROGRAM pointerComponent01
    USE dMod

    IMPLICIT NONE

    INTEGER(4), TARGET :: array2( 2 ) = [ 99_4, 98_4 ]

    INTEGER(4), TARGET :: array3_1( 3 ) = [ 1_4, 2_4, 3_4 ]
    INTEGER(4), TARGET :: array3_2( 3 ) = [ -99_4, -98_4, -97_4 ]

    TYPE(contained(2,4)), TARGET :: jar

    TYPE(contained(3,4)), TARGET :: tub
    TYPE(contained(3,4)), TARGET :: cup

    TYPE(base(4,2)) :: bObj
    TYPE(derived(4,3)) :: dObj


    jar = contained(2,4)(array2)

    tub = contained(3,4)(array3_1)
    cup = contained(3,4)(array3_2)


    bObj%cBasePtr => jar

    ASSOCIATE(b => bObj)

        IF (b%k /= KIND( array2 )) THEN
            STOP 10

        ELSE IF (b%cBasePtr%k /= KIND( array2 )) THEN
            STOP 11

        ELSE IF (KIND( b%cBasePtr%value ) /= KIND( array2 )) THEN
            STOP 12


        ELSE IF (b%l /= SIZE( array2 )) THEN
            STOP 15

        ELSE IF (b%cBasePtr%l /= SIZE( array2 )) THEN
            STOP 16

        ELSE IF (SIZE( b%cBasePtr%value ) /= SIZE( array2 )) THEN
            STOP 17


        ELSE IF ( ANY(b%cBasePtr%value /= array2) ) THEN
            STOP 20
        END IF

    END ASSOCIATE


    dObj%cBasePtr => tub
    dObj%cDerivedPtr => cup

    ASSOCIATE(d => dObj)

        IF (d%k /= KIND( array3_1 )) THEN
            STOP 30


        ELSE IF (d%cBasePtr%k /= KIND( array3_1 )) THEN
            STOP 35

        ELSE IF (KIND( d%cBasePtr%value ) /= KIND( array3_1 )) THEN
            STOP 36


        ELSE IF (d%cDerivedPtr%k /= KIND( array3_2 )) THEN
            STOP 40

        ELSE IF (KIND( d%cDerivedPtr%value ) /= KIND( array3_2 )) THEN
            STOP 41


        ELSE IF (d%l /= SIZE( array3_1 )) THEN
            STOP 45


        ELSE IF (d%cBasePtr%l /= SIZE( array3_1 )) THEN
            STOP 50

        ELSE IF (SIZE( d%cBasePtr%value ) /= SIZE( array3_1 )) THEN
            STOP 51


        ELSE IF (d%cDerivedPtr%l /= SIZE( array3_2 )) THEN
            STOP 55

        ELSE IF (SIZE( d%cDerivedPtr%value ) /= SIZE( array3_2 )) THEN
            STOP 56


        ELSE IF ( ANY(d%cBasePtr%value /= array3_1) ) THEN
            STOP 60

        ELSE IF ( ANY(d%cDerivedPtr%value /= array3_2) ) THEN
            STOP 61
        END IF

    END ASSOCIATE

END PROGRAM pointerComponent01
