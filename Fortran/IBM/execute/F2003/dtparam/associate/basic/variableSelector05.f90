!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : July  8, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is a variable
!*  SECONDARY FUNCTIONS TESTED : variable is an Unlimited Polymorphic
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Basic Testing where selector is:
!*  o  A Polymorphic variable of a Derived Type that requires Type Parameters
!*     (an Unlimited Polymorphic)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mod
    IMPLICIT NONE

    TYPE base1(k1)
        INTEGER, KIND :: k1

        INTEGER :: k1Val = k1
    END TYPE base1

    TYPE, EXTENDS(base1) :: extended1(k2)
        INTEGER, KIND :: k2

        INTEGER :: k2Val = k2
    END TYPE extended1

    TYPE base2(l1)
        INTEGER, LEN :: l1

        INTEGER :: l1Val != l1
    END TYPE base2

    TYPE, EXTENDS(base2) :: extended2(l2)
        INTEGER, LEN :: l2

        INTEGER :: l2Val != l2
    END TYPE extended2

END MODULE mod


PROGRAM variableSelector05
    USE mod

    IMPLICIT NONE

    INTERFACE
        RECURSIVE SUBROUTINE CheckTypeParameters(rc, obj, p1, p2)
            INTEGER(4) :: rc
            CLASS(*) :: obj
            INTEGER :: p1
            INTEGER, OPTIONAL :: p2
        END SUBROUTINE CheckTypeParameters
    END INTERFACE


    TYPE(base1(4)) :: base1
    TYPE(extended1(4,8)) :: extended1
    TYPE(base2(11)) :: base2 = base2(11)(11)
    TYPE(extended2(13,0)) :: extended2 = extended2(13,0)(13,0)


    CALL CheckTypeParameters(10_4, base1, 4)
    CALL CheckTypeParameters(20_4, extended1, 4, 8)

    CALL CheckTypeParameters(40_4, base2, 11)
    CALL CheckTypeParameters(50_4, extended2, 13, 0)

END PROGRAM variableSelector05


RECURSIVE SUBROUTINE CheckTypeParameters(rc, obj, p1, p2)
    USE mod

    IMPLICIT NONE

    INTEGER(4) :: rc
    CLASS(*) :: obj
    INTEGER :: p1
    INTEGER, OPTIONAL :: p2


    PRINT *

    ASSOCIATE(o => obj)
        SELECT TYPE ( o )
            CLASS IS (base1(4))
                PRINT *, 'CLASS(base1(', o%k1Val, ")", p1

                IF (o%k1Val /= p1) THEN
                    CALL zzrc( rc )
                END IF

                SELECT TYPE ( o )
                    TYPE IS (base1(4))
                        PRINT *, '    TYPE(base1(', o%k1Val, ")"

                    TYPE IS (extended1(4,8))
                        PRINT *, '    TYPE(extended1(',&
                                    o%k1Val, ',', o%k2Val, ')', p2

                        IF (o%k2Val /= p2) THEN
                            CALL zzrc( (rc + 2_4) )
                        END IF

                        CALL CheckTypeParameters((rc + 10_4), o%base1, p1)

                    CLASS DEFAULT
                        PRINT *, '    TYPE(Unknown)'
                        CALL zzrc( (rc + 3_4) )
                END SELECT

            CLASS IS (base2(*))
                PRINT *, 'CLASS(base2(', o%l1Val, ")", p1

                IF (o%l1Val /= p1) THEN
                    CALL zzrc( (rc + 4_4) )
                END IF

                SELECT TYPE ( o )
                    TYPE IS (base2(*))
                        PRINT *, '    TYPE(base2(', o%l1Val, ")", p1

                    TYPE IS (extended2(*,*))
                        PRINT *, '    TYPE(extended2(',&
                                    o%l1Val, ',', o%l2Val, ')', p2

                        IF (o%l2Val /= p2) THEN
                            CALL zzrc( (rc + 5_4) )
                        END IF

                        CALL CheckTypeParameters((rc + 10_4), o%base2, p1)

                    CLASS DEFAULT
                        PRINT *, '    TYPE(Unknown)'
                        CALL zzrc( (rc + 6_4) )
                END SELECT

            CLASS DEFAULT
                PRINT *, 'TYPE(Unknown)'
                CALL zzrc( (rc + 7_4) )
        END SELECT
    END ASSOCIATE

END SUBROUTINE CheckTypeParameters
