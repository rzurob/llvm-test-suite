!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : nestedAssociate01
!*
!*  DATE                       : August 22, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : The ASSOCIATE Construct
!*  SECONDARY FUNCTIONS TESTED : Multiple Nesting of ASSOCIATE Constructs
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Miscellaneous Testing:
!*  * An ASSOCIATE Construct contained within an ASSOCIATE Consturct (3
!*    Levels of Constructs).
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE baseMod
    IMPLICIT NONE

    TYPE base(l)
        INTEGER, LEN :: l

        CHARACTER(l) :: str
    END TYPE base

END MODULE baseMod


MODULE nestMod
    USE baseMod

    IMPLICIT NONE

    TYPE, EXTENDS(base) :: nest(k)
        INTEGER, KIND :: k

        INTEGER(k) :: i
    END TYPE nest

END MODULE nestMod


PROGRAM nestedAssociate01
    USE baseMod
    USE nestMod

    IMPLICIT NONE

    CHARACTER(5), PARAMETER :: title = 'title'
    CHARACTER(3), PARAMETER :: bar = 'bar'

    TYPE(base(5)) :: baseVar = base(5)(title)
    TYPE(nest(3,8)) :: nestVar = nest(3,8)(bar,33_8)


    ASSOCIATE(b1 => baseVar)
        CALL CheckBase(b1, title, 10_4)

        ASSOCIATE(b2 => b1)
            CALL CheckBase(b1, title, 20_4)
            CALL CheckBase(b2, title, 25_4)

            ASSOCIATE(b3 => b2)
                CALL CheckBase(b1, title, 30_4)
                CALL CheckBase(b2, title, 33_4)
                CALL CheckBase(b3, title, 37_4)
            END ASSOCIATE

        END ASSOCIATE

    END ASSOCIATE


    ASSOCIATE(n1 => nestVar)
        CALL CheckNest(n1, bar, 33_8, 40_4)

        ASSOCIATE(n2 => n1)
            CALL CheckNest(n1, bar, 33_8, 50_4)
            CALL CheckNest(n2, bar, 33_8, 60_4)

            ASSOCIATE(n3 => n2)
                CALL CheckNest(n1, bar, 33_8, 70_4)
                CALL CheckNest(n2, bar, 33_8, 80_4)
                CALL CheckNest(n3, bar, 33_8, 90_4)
            END ASSOCIATE

        END ASSOCIATE

    END ASSOCIATE


    CONTAINS

        SUBROUTINE CheckBase(b, txt, rc)
            TYPE(base(*)) :: b
            CHARACTER(*) :: txt
            INTEGER(4) :: rc

            PRINT *, LEN( b%str ), "(", b%str, ")", LEN( txt ), "(", txt, ")"
            IF (LEN( b%str ) /= LEN( txt )) THEN
                CALL zzrc( rc )

            ELSE IF (b%l /= LEN( txt )) THEN
                CALL zzrc( (rc + 1_4) )

            ELSE IF (b%str /= txt) THEN
                CALL zzrc( (rc + 2_4) )
            END IF

        END SUBROUTINE CheckBase

        SUBROUTINE CheckNest(n, txt, i, rc)
            TYPE(nest(*,8)) :: n
            CHARACTER(*) :: txt
            INTEGER(8) :: i
            INTEGER(4) :: rc

            PRINT *, LEN( n%str ), "(", n%str, ")", LEN( txt ), "(", txt, ")"
            PRINT *, LEN( n%base%str ),"(",n%base%str,")",LEN( txt ),"(",txt,")"
            IF (n%l /= LEN( txt )) THEN
                CALL zzrc( rc )

            ELSE IF (KIND( n%i ) /= 8) THEN
                CALL zzrc( (rc + 1_4) )

            ELSE IF (n%k /= 8) THEN
                CALL zzrc( (rc + 2_4) )

            ELSE IF (n%i /= i) THEN
                CALL zzrc( (rc + 3_4) )
            END IF

            CALL CheckBase(n%base, txt, (rc + 4_4))

        END SUBROUTINE CheckNest

END PROGRAM nestedAssociate01
