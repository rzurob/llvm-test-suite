!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : deferedVariableSelector03
!*                               Parameter(s)
!*
!*  DATE                       : August  1, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is a variable
!*  SECONDARY FUNCTIONS TESTED : The Variable is an Unlimited Polymorphic
!*                               and has a Defered Length Parameter
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Using the Basic Testing (as defined above):
!*  o selector has Defered Length Type Parameters
!*    - A Polymorphic variable of a Derived Type that requires Type Parameters
!*      (an Unlimited Polymorphic)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE TestData
    IMPLICIT NONE

    INTEGER :: testNum = 0

    CHARACTER(3), PARAMETER :: ibm = 'IBM'
    CHARACTER(5), PARAMETER :: hello = 'Hello'

    REAL(16), PARAMETER :: real16( 1 ) = [ 3.14_16 ]

END MODULE TestData


MODULE Madness
    IMPLICIT NONE

    TYPE object(ell1)
        INTEGER(4), LEN :: ell1

        CHARACTER(ell1) :: string
    END TYPE object

    TYPE, EXTENDS(object) :: method(ell2,k)
        INTEGER(2), LEN :: ell2
        INTEGER(8), KIND :: k

        REAL(k) :: array( ell2 )
    END TYPE method

    CLASS(*), ALLOCATABLE :: baseObj

    CONTAINS

        SUBROUTINE BaseCheck( rc )
            USE TestData

            INTEGER(4) :: rc

            CHARACTER(:), ALLOCATABLE :: checkStr


            testNum = testNum + 1

            IF (testNum == 1) THEN
                ALLOCATE(checkStr, SOURCE=hello)

            ELSE IF (testNum == 2) THEN
                ALLOCATE(checkStr, SOURCE=ibm)

            ELSE
                CALL zzrc( rc )
            END IF


            ASSOCIATE(b => baseObj)
                SELECT TYPE ( b )
                    CLASS IS (object(*))
                        IF (LEN( b%string ) /= LEN( checkStr )) THEN
                            CALL zzrc( (rc + 1_4) )

                        ELSE IF (b%ell1 /= LEN( checkStr )) THEN
                            CALL zzrc( (rc + 2_4) )

                        ELSE IF (b%string /= checkStr) THEN
                            CALL zzrc( (rc + 3_4) )
                        END IF

                    CLASS DEFAULT
                        CALL zzrc( (rc + 4_4) )
                END SELECT
            END ASSOCIATE

            DEALLOCATE( checkStr )

        END SUBROUTINE BaseCheck

END MODULE Madness


PROGRAM deferedVariableSelector03
    USE TestData
    USE Madness

    IMPLICIT NONE


    INTERFACE
        SUBROUTINE DerivedCheck(derived, rc)
            USE madness
            IMPLICIT NONE

            CLASS(*) :: derived
            INTEGER(4) :: rc
        END SUBROUTINE DerivedCheck
    END INTERFACE


    TYPE(object(:)), ALLOCATABLE :: objectVar
    TYPE(method(:,:,KIND( real16 ))), ALLOCATABLE :: methodVar


    ALLOCATE(objectVar, SOURCE=object(LEN( hello ))(hello))
    ALLOCATE(baseObj, SOURCE=objectVar)

    CALL BaseCheck( 10_4 )

    DEALLOCATE( baseObj )
    DEALLOCATE( objectVar )


    ALLOCATE(methodVar,&
        SOURCE=method(LEN( ibm ),SIZE( real16 ),KIND( real16 ))(ibm,real16))

    CALL DerivedCheck(methodVar, 20_4)

    DEALLOCATE( methodVar )

END PROGRAM deferedVariableSelector03


SUBROUTINE DerivedCheck(derived, rc)
    USE TestData
    USE Madness

    IMPLICIT NONE

    CLASS(*) :: derived
    INTEGER(4) :: rc


    ALLOCATE(baseObj, SOURCE=derived)

    CALL BaseCheck( (rc + 10_4) )

    DEALLOCATE( baseObj )


    ASSOCIATE(d => derived)
        SELECT TYPE ( d )
            TYPE IS (method(*,*,KIND( real16 )))
                IF (KIND( d%array ) /= KIND( real16 )) THEN
                    CALL zzrc( rc )

                ELSE IF (d%k /= KIND( real16 )) THEN
                    CALL zzrc( (rc + 1_4) )

                ELSE IF (SIZE( d%array ) /= SIZE( real16 )) THEN
                    CALL zzrc( (rc + 2_4) )

                ELSE IF (d%ell2 /= SIZE( real16 )) THEN
                    CALL zzrc( (rc + 3_4) )

                ELSE IF ( ANY(d%array /= real16) ) THEN
                    CALL zzrc( (rc + 4_4) )
                END IF

            CLASS DEFAULT
                CALL zzrc( (rc + 5_4) )
        END SELECT
    END ASSOCIATE

END SUBROUTINE DerivedCheck
