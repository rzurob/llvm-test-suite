!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : exprSelector07
!*  TEST CASE TITLE            : expr selector with Derived Type Parameters
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : August 13, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is an expr
!*  SECONDARY FUNCTIONS TESTED : The Expression references a Procedure Pointer
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Basic Testing where selector is:
!*  * An expr that contains a:
!*    - Single primary (a Procedure Pointer)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE baseMod
    IMPLICIT NONE

    TYPE base(l)
        INTEGER, LEN :: l
    END TYPE base


    ABSTRACT INTERFACE
        FUNCTION absFunc(arg, rc)
            IMPORT base
            CLASS(base(*)), TARGET :: arg
            INTEGER(4) :: rc

            CLASS(base(:)), POINTER :: absFunc
        END FUNCTION absFunc
    END INTERFACE


    PROCEDURE(absFunc), POINTER :: baseProcPtr => NULL( )

    CONTAINS

        FUNCTION ReturnBase(arg, rc)
            CLASS(base(*)), TARGET :: arg
            INTEGER(4) :: rc

            CLASS(base(:)), POINTER :: ReturnBase

            ReturnBase => NULL( )
            SELECT TYPE ( arg )
                CLASS IS (base(*))
                    ReturnBase => arg

                CLASS DEFAULT
                    CALL zzrc( rc )
            END SELECT

        END FUNCTION ReturnBase

END MODULE baseMod

MODULE derivedMod
    USE baseMod

    IMPLICIT NONE

    TYPE, EXTENDS(base) :: derived(k)
        INTEGER, KIND :: k
    END TYPE derived

    CONTAINS

        FUNCTION ReturnDerived(arg, rc)
            CLASS(base(*)), TARGET :: arg
            INTEGER(4) :: rc

            CLASS(base(:)), POINTER :: ReturnDerived

            ReturnDerived => NULL( )
            SELECT TYPE ( arg )
                CLASS IS (derived(*,8))
                    ReturnDerived => arg

                CLASS DEFAULT
                    CALL zzrc( rc )
            END SELECT

        END FUNCTION ReturnDerived

END MODULE derivedMod


PROGRAM exprSelector07
    USE derivedMod

    IMPLICIT NONE

    TYPE(base(7)), TARGET :: b
    TYPE(derived(11,8)), TARGET :: d


    baseProcPtr => ReturnBase
    ASSOCIATE(basePtr => baseProcPtr(b, 10_4))

        SELECT TYPE ( basePtr )
            TYPE IS (base(*))
                IF (basePtr%l /= 7) THEN
                    CALL zzrc( 20_4 )
                END IF

            CLASS DEFAULT
                CALL zzrc( 30_4 )
        END SELECT

    END ASSOCIATE


    ASSOCIATE(basePtr => baseProcPtr(d, 40_4))

        SELECT TYPE ( basePtr )
            CLASS IS (base(*))
                IF (basePtr%l /= 11) THEN
                    CALL zzrc( 50_4 )
                END IF

            CLASS DEFAULT
                CALL zzrc( 60_4 )
        END SELECT

    END ASSOCIATE


    baseProcPtr => ReturnDerived
    ASSOCIATE(derivedPtr => baseProcPtr(d, 70_4))

        SELECT TYPE ( derivedPtr )
            TYPE IS (derived(*,8))
                IF (derivedPtr%l /= 11) THEN
                    CALL zzrc( 80_4 )
                END IF

            CLASS DEFAULT
                CALL zzrc( 90_4 )
        END SELECT

    END ASSOCIATE

END PROGRAM exprSelector07
