!***********************************************************************
!* =====================================================================
!*
!*                               Component
!*
!*  DATE                       : August 19, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is a variable
!*  SECONDARY FUNCTIONS TESTED : The Variable has a Polymorphic Component
!*                               with the ALLOCATABLE Attribute
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
!*    o An ALLOCATABLE Polymorphic Component
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE base
    IMPLICIT NONE

    TYPE b(l1,k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        INTEGER(k1) :: loci( l1 )
    END TYPE b

END MODULE base


MODULE derived
    USE base

    IMPLICIT NONE

    TYPE, EXTENDS(b) :: d
        INTEGER(k1) :: gucci( l1 )
    END TYPE d

END MODULE derived


MODULE collection
    USE derived

    IMPLICIT NONE

    TYPE c(k,l)
        INTEGER, KIND :: k
        INTEGER, LEN :: l

        CLASS(b(l,k)), ALLOCATABLE :: group
    END TYPE c

END MODULE collection


PROGRAM polymorphicComponent01
    USE collection

    IMPLICIT NONE

    TYPE(b(5,4)), POINTER :: bPointer
    TYPE(d(5,4)), POINTER :: dPointer

    INTEGER(4), TARGET :: array1( 5 ) = [ 1_4, 2_4, 3_4, 4_4, 5_4 ]
    INTEGER(4), TARGET :: array2( 5 ) = [ 99_4, 98_4, 97_4, 96_4, 95_4 ]

    INTEGER(4), POINTER :: p1Verify( : )
    INTEGER(4), POINTER :: p2Verify( : )

    TYPE(c(4,5)) :: organization


    p1Verify => array1
    p2Verify => array2

    organization = c(4,5)(d(5,4)(array1,array2))
    CALL CheckOrganization( 10_4 )


    p1Verify => array2

    organization = c(4,5)(b(5,4)(array2))
    CALL CheckOrganization( 50_4 )


    CONTAINS

        SUBROUTINE CheckOrganization( rc )
            INTEGER(4) :: rc

            ASSOCIATE(org => organization)

                SELECT TYPE (g => org%group)
                    TYPE IS (d(*,4))
                        dPointer => g
                        bPointer => g%b

                        CALL CheckDerived( (rc + 10_4) )

                    TYPE IS (b(*,4))
                        bPointer => g

                        CALL CheckBase( (rc + 10_4) )

                    CLASS DEFAULT
                        CALL zzrc( rc )
                END SELECT

            END ASSOCIATE

        END SUBROUTINE CheckOrganization

        SUBROUTINE CheckDerived( rc )
            INTEGER(4) :: rc

            IF (KIND( dPointer%gucci ) /= KIND( p2Verify )) THEN
                CALL zzrc( rc )

            ELSE IF (dPointer%k1 /= KIND( p2Verify )) THEN
                CALL zzrc( (rc + 1_4) )

            ELSE IF (SIZE( dPointer%gucci ) /= SIZE( p2Verify )) THEN
                CALL zzrc( (rc + 2_4) )

            ELSE IF (dPointer%l1 /= SIZE( p2Verify )) THEN
                CALL zzrc( (rc + 3_4) )

            ELSE IF ( ANY(dPointer%gucci /= p2Verify) ) THEN
                CALL zzrc( (rc + 4_4) )
            END IF

            CALL CheckBase( (rc + 5_4) )

        END SUBROUTINE CheckDerived

        SUBROUTINE CheckBase( rc )
            INTEGER(4) :: rc

            IF (KIND( bPointer%loci ) /= KIND( p1Verify )) THEN
                CALL zzrc( rc )

            ELSE IF (bPointer%k1 /= KIND( p1Verify )) THEN
                CALL zzrc( (rc + 1_4) )

            ELSE IF (SIZE( bPointer%loci ) /= SIZE( p1Verify )) THEN
                CALL zzrc( (rc + 2_4) )

            ELSE IF (bPointer%l1 /= SIZE( p1Verify )) THEN
                CALL zzrc( (rc + 3_4) )

            ELSE IF ( ANY(bPointer%loci /= p1Verify) ) THEN
                CALL zzrc( (rc + 4_4) )
            END IF

        END SUBROUTINE CheckBase

END PROGRAM polymorphicComponent01
