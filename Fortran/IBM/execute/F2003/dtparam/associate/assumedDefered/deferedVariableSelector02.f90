!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : deferedVariableSelector02
!*                               Parameter(s)
!*
!*  DATE                       : July 23, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is a variable
!*  SECONDARY FUNCTIONS TESTED : The Variable is Polymorphic and has a
!*                               Defered Length Parameter
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Using the Basic Testing (as defined above):
!*  o selector has Assumed/Defered Length Type Parameters
!*    - A Polymorphic variable of a Derived Type that requires Type Parameters
!*      (Base Type of an Extended Derived Type, and an Extended Derived Type)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE mod1
    IMPLICIT NONE

    TYPE base(l1)
        INTEGER, LEN :: l1

        COMPLEX :: cArray( l1 )
    END TYPE base

    TYPE, EXTENDS(base) :: extended(k1)
        INTEGER, KIND :: k1

        REAL(k1) :: rArray( l1 )
    END TYPE extended


    COMPLEX, ALLOCATABLE :: baseExpected( : )
    REAL(4), ALLOCATABLE :: extendedExpected( : )

    CONTAINS

        SUBROUTINE VerifyBase(baseArg, rc)
            CLASS(base(:)), POINTER :: baseArg
            INTEGER(4) :: rc


            PRINT *, "VerifyBase(", rc, ")"

            ASSOCIATE(ba => baseArg)
                IF (SIZE( ba%cArray ) /= SIZE( baseExpected )) THEN
                    PRINT *, "SIZE( ba%cArray ) =", SIZE( ba%cArray )
                    PRINT *, "SIZE( baseExpected ) =", SIZE( baseExpected )

                    CALL zzrc( rc )

                ELSE IF ( ANY(ba%cArray /= baseExpected) ) THEN
                    PRINT *, "ba%cArray = (", ba%cArray, ")"
                    PRINT *, "baseExpected = (", baseExpected, ")"

                    CALL zzrc( (rc + 1_4) )
                END IF
            END ASSOCIATE

        END SUBROUTINE VerifyBase

END MODULE mod1


MODULE mod2
    USE mod1
    IMPLICIT NONE

    TYPE(base(3)), TARGET :: base_3 =&
        base(3)([ (1.0,1.0), (2.0,2.0), (3.0,3.0) ])

END MODULE mod2


PROGRAM deferedVariableSelector02
    USE mod1
    USE mod2
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE VerifyExtended(extendedArg, rc)
            USE mod1
            IMPLICIT NONE

            CLASS(extended(:,4)) :: extendedArg
            INTEGER(4) :: rc
        END SUBROUTINE VerifyExtended
    END INTERFACE

    CLASS(base(:)), POINTER :: basePoly
    CLASS(extended(:,4)), POINTER :: extendedPoly


    ALLOCATE(baseExpected( 3 ), SOURCE=base_3%cArray)

    basePoly => base_3
    CALL VerifyBase(basePoly, 10_4)

    ASSOCIATE(bp => base_3)
        CALL VerifyBase(basePoly, 20_4)
    END ASSOCIATE

    DEALLOCATE( baseExpected )


    CALL DefineExtended( 90_4 )


    CONTAINS

        SUBROUTINE DefineExtended( rc )
            INTEGER(4) :: rc

            TYPE(extended(2,4)), TARGET :: extended_2_4 =&
                 extended(2,4)([ (4.0,4.0),(5.0,5.0) ],[ 6.0_4, 7.0_4 ])

            CLASS(base(:)), POINTER :: basePolyPtr


            PRINT *, "DefineExtended(", rc, ")"

            ALLOCATE(baseExpected( 2 ), SOURCE=extended_2_4%cArray)

            basePoly => extended_2_4
            CALL VerifyBase(basePoly, rc)

            ASSOCIATE(bP => basePoly)
                basePolyPtr => bP
                CALL VerifyBase(basePolyPtr, (rc + 10_4))
            END ASSOCIATE

            basePoly => extended_2_4%base
            CALL VerifyBase(basePoly, (rc + 20_4))

            ASSOCIATE(bP => basePoly)
                basePolyPtr => bP
                CALL VerifyBase(basePolyPtr, (rc + 30_4))
            END ASSOCIATE


            ALLOCATE(extendedExpected( 2 ), SOURCE=extended_2_4%rArray)

            extendedPoly => extended_2_4

            basePoly => extendedPoly%base
            PRINT *, "(", basePoly%cArray, ")"
            CALL VerifyBase(basePoly, (rc + 40_4))

            ASSOCIATE(bP => basePoly)
                basePolyPtr => bP
                CALL VerifyBase(basePolyPtr, (rc + 50_4))
            END ASSOCIATE

            CALL VerifyExtended(extendedPoly, (rc + 60_4))

            ASSOCIATE(eP => extendedPoly)
                CALL VerifyExtended(eP, (rc + 100_4))
            END ASSOCIATE

            DEALLOCATE( extendedExpected )
            DEALLOCATE( baseExpected )

        END SUBROUTINE DefineExtended

END PROGRAM deferedVariableSelector02


SUBROUTINE VerifyExtended(extendedArg, rc)
    USE mod1
    IMPLICIT NONE

    CLASS(extended(:,4)), POINTER :: extendedArg
    INTEGER(4) :: rc

    CLASS(base(:)), POINTER :: bp


    PRINT *, "VerifyExtended(", rc, ")"

    bp => extendedArg
    CALL VerifyBase(bp, (rc + 10_4))

    bp => extendedArg%base
    CALL VerifyBase(bp, (rc + 20_4))

    ASSOCIATE(ea => extendedArg)
        SELECT TYPE ( ea )
            CLASS IS (extended(*,4))
                IF (SIZE( ea%rArray ) /= SIZE( extendedExpected )) THEN
                    PRINT *, "SIZE( ea%rArray ) =", SIZE( ea%rArray )
                    PRINT *, "SIZE( extendedExpected ) =",&
                              SIZE( extendedExpected )

                    CALL zzrc( rc )

                ELSE IF ( ANY(ea%rArray /= extendedExpected) ) THEN
                    PRINT *, "ea%rArray = (", ea%rArray, ")"
                    PRINT *, "extendedExpected = (", extendedExpected, ")"

                    CALL zzrc( (rc + 1_4) )
                END IF

                bp => ea
                CALL VerifyBase(bp, (rc + 30_4))

                bp => ea%base
                CALL VerifyBase(bp, (rc + 40_4))

            CLASS DEFAULT
                CALL zzrc( (rc + 2_4) )
        END SELECT
    END ASSOCIATE

END SUBROUTINE VerifyExtended
