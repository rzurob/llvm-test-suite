!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : July  8, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is a variable
!*  SECONDARY FUNCTIONS TESTED : variable is a Polymorphic Base Type of a
!*                               Derived Type (with Type Parameters)
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
!*     (Base Type of an Extended Derived Type)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

PROGRAM variableSelector03
    IMPLICIT NONE

    TYPE base(l1, k1)
        INTEGER, LEN :: l1
        INTEGER, KIND :: k1

        REAL(k1) :: array( l1 )
    END TYPE base

    TYPE, EXTENDS(base) :: extended1(l2)
        INTEGER, LEN :: l2

        INTEGER :: ext1Array( l2 )
    END TYPE extended1

    TYPE, EXTENDS(base) :: extended2(k2)
        INTEGER, KIND :: k2

        INTEGER :: ext2Array( k2 )
    END TYPE extended2

    TYPE(base(3,8)), TARGET :: base
    TYPE(extended1(1,8,7)), TARGET :: extended1
    TYPE(extended2(2,8,4)), TARGET :: extended2

    CLASS(base(:,8)), POINTER :: basePtr


    basePtr => base
    CALL CheckTypeParameters(10_4, 3, 8)

    basePtr => extended1
    CALL CheckTypeParameters(20_4, 1, 8, 7)

    basePtr => extended2
    CALL CheckTypeParameters(30_4, 2, 8, 4)


    CONTAINS

        SUBROUTINE CheckTypeParameters(rc, p1, p2, p3)
            INTEGER(4) :: rc
            INTEGER :: p1
            INTEGER :: p2
            INTEGER, OPTIONAL :: p3


            PRINT *

            ASSOCIATE(pB => basePtr)
                PRINT *, "SIZE(pB%array) =", SIZE( pB%array ), "(", p1, ")"
                PRINT *, "    KIND(pB%array) =", KIND( pB%array ), "(", p2, ")"

                IF (SIZE( pB%array ) /= p1) THEN
                    CALL zzrc( rc )

                ELSE IF (KIND( pB%array ) /= p2) THEN
                    CALL zzrc( (rc + 1_4) )
                END IF

                SELECT TYPE ( pB )
                    TYPE IS (base(*,8))
                        PRINT *, '    pB is TYPE(base)'

                    TYPE IS (extended1(*,8,*))
                        PRINT *, "    SIZE(pB%ext1Array) =",&
                                SIZE( pB%ext1Array ), "(", p3, ")"
                        PRINT *, '    pB is TYPE(extended1)'

                        IF (SIZE( pB%ext1Array ) /= p3) THEN
                            CALL zzrc( (rc + 2_4) )
                        END IF

                    TYPE IS (extended2(*,8,4))
                        PRINT *, "    SIZE(pB%ext2Array) =",&
                                SIZE( pB%ext2Array ), "(", p3, ")"
                        PRINT *, '    pB is TYPE(extended2)'

                        IF (SIZE( pB%ext2Array ) /= p3) THEN
                            CALL zzrc( (rc + 3_4) )
                        END IF

                    CLASS DEFAULT
                        PRINT *, '    pB is TYPE(Unknown)'
                        CALL zzrc( (rc + 4_4) )

                END SELECT
            END ASSOCIATE

        END SUBROUTINE CheckTypeParameters

END PROGRAM variableSelector03
