!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : componentSelector01
!*                               Type
!*
!*  DATE                       : August 29, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is the Component of a variable
!*                               of Dervied Type
!*  SECONDARY FUNCTIONS TESTED : The Component has no Attributes
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Testing where the selector is a Component of a Derived Type that uses
!*  Type Parameters, and the Component:
!*  * Has no Component Attributes,
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE tListMod
    IMPLICIT NONE

    TYPE tList(l1,k,l2)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k
        INTEGER, LEN :: l2

        CHARACTER(l1) :: listType
    END TYPE tList

END MODULE tListMod

MODULE tIntListMod
    USE tListMod

    IMPLICIT NONE

    TYPE, EXTENDS(tList) :: tIntList
        INTEGER(k) :: list( l2 )
    END TYPE tIntList

END MODULE tIntListMod


PROGRAM componentSelector01
    USE tIntListMod

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j
    INTEGER :: k

    INTEGER(4) :: l

    CHARACTER(4) :: base = 'base'
    CHARACTER(7) :: intText = 'integer'

    TYPE(tList(4,2,0)) :: baseList
    TYPE(tIntList(7,2,5)) :: listInts( 5 )


    baseList = tList(4,2,0)(base)
    listInts =&
        [ (tIntList(7,2,5)(intText,[ ((i + j), j = 0, 4) ]), i = 5, 25, 5) ]


    ASSOCIATE(baseListType => baseList%listType)

        IF (LEN( baseListType ) /= LEN( baseList%listType )) THEN
            CALL zzrc( 10_4 )

        ELSE IF (LEN( baseListType ) /= baseList%l1) THEN
            CALL zzrc( 11_4 )

        ELSE IF (LEN( baseListType ) /= LEN( base )) THEN
            CALL zzrc( 12_4 )

        ELSE IF (LEN( baseListType ) /= 4) THEN
            CALL zzrc( 13_4 )

        ELSE IF (baseListType /= baseList%listType) THEN
            CALL zzrc( 14_4 )

        ELSE IF (baseListType /= base) THEN
            CALL zzrc( 15_4 )
        END IF

    END ASSOCIATE


    DO j = 5, 25, 5

        k = j / 5
        l = INT(k, 4) * 20_4

        ASSOCIATE(intListType => listInts( k )%listType)

            IF (LEN( intListType ) /= LEN( listInts( k )%listType )) THEN
                CALL zzrc( l )

            ELSE IF (LEN( intListType ) /= listInts( k )%l1) THEN
                CALL zzrc( (l + 1_4) )

            ELSE IF (LEN( intListType ) /= LEN( intText )) THEN
                CALL zzrc( (l + 2_4) )

            ELSE IF (LEN( intListType ) /= 7) THEN
                CALL zzrc( (l + 3_4) )

            ELSE IF (intListType /= listInts( k )%listType) THEN
                CALL zzrc( (l + 4_4) )

            ELSE IF (intListType /= intText) THEN
                CALL zzrc( (l + 5_4) )
            END IF

        END ASSOCIATE

        ASSOCIATE(theIntList => listInts( (j / 5) )%list)

            IF (KIND( theIntList ) /= KIND( listInts( k )%list )) THEN
                CALL zzrc( (l + 10_4) )

            ELSE IF (KIND( theIntList ) /= listInts( k )%k) THEN
                CALL zzrc( (l + 11_4) )

            ELSE IF (KIND( theIntList ) /= 2) THEN
                CALL zzrc( (l + 12_4) )

            ELSE IF (SIZE( theIntList ) /= SIZE( listInts( k )%list )) THEN
                CALL zzrc( (l + 13_4) )

            ELSE IF (SIZE( theIntList ) /= listInts( k )%l2) THEN
                CALL zzrc( (l + 14_4) )

            ELSE IF (SIZE( theIntList ) /= 5) THEN
                CALL zzrc( (l + 15_4) )

            ELSE IF ( ANY(theIntList /= listInts( k )%list) ) THEN
                CALL zzrc( (l + 16_4) )
            END IF

        END ASSOCIATE

    END DO

END PROGRAM componentSelector01
