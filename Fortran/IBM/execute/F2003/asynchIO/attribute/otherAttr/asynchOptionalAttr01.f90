!*  ===================================================================
!*
!*                               Interactions with Other Attributes
!*
!*  DATE                       : February  9, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS Attribute
!*  SECONDARY FUNCTIONS TESTED : Interactions with the OPTIONAL Attribute
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  5.1 Type declaration statements
!*
!*  R501 type-declaration-stmt  is  declaration-type-spec [ [ , attr-spec ]&
!*                                      &... :: ] entity-decl-list
!*
!*  R502 declaration-type-spec is intrinsic-type-spec
!*
!*  R503 attr-spec  is  access-spec
!*                  or  ALLOCATABLE
!*                  or  ASYNCHRONOUS
!*  ...
!*                  or OPTIONAL
!*
!*  5.1.2.9 OPTIONAL attribute
!*
!*  The OPTIONAL attribute specifies that the dummy argument need not be
!*  associated with an actual argument in a reference to the procedure
!*  (12.4.1.6). The PRESENT intrinsic function may be used to determine
!*  whether an actual argument has been associated with a dummy argument
!*  having the OPTIONAL attribute.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM asynchOptionalAttr01
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE OptionalArg( optArg )
            IMPLICIT NONE
            COMPLEX, OPTIONAL :: optArg
        END SUBROUTINE OptionalArg

        SUBROUTINE OptionalArgAsynch( optArgAsynch )
            IMPLICIT NONE
            COMPLEX, OPTIONAL, ASYNCHRONOUS :: optArgAsynch
        END SUBROUTINE OptionalArgAsynch
    END INTERFACE

    COMPLEX :: complexArg = (1.0,1.0)


    CALL OptionalArg( )
    CALL OptionalArg( complexArg )


    CALL OptionalArgAsynch( )
    CALL OptionalArgAsynch( complexArg )

END PROGRAM asynchOptionalAttr01


SUBROUTINE OptionalArg( optArg )
    IMPLICIT NONE

    COMPLEX, OPTIONAL :: optArg

    IF ( PRESENT( optArg ) ) THEN
        WRITE(6, 100) optArg
100     FORMAT(' OptionalArg():              optArg = (',F3.1,',',F3.1,')')

    ELSE
        WRITE(6, *) "OptionalArg():              optArg = NotPresent"
    END IF

END SUBROUTINE OptionalArg


SUBROUTINE OptionalArgAsynch( optArgAsynch )
    IMPLICIT NONE

    COMPLEX, OPTIONAL, ASYNCHRONOUS :: optArgAsynch

    IF ( PRESENT( optArgAsynch ) ) THEN
        WRITE(6, 100) optArgAsynch
100     FORMAT(' OptionalArgAsynch():  optArgAsynch = (',F3.1,',',F3.1,')')

    ELSE
        WRITE(6, *) "OptionalArgAsynch():  optArgAsynch = NotPresent"
    END IF

END SUBROUTINE OptionalArgAsynch
