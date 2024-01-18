!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : valueAttrUserFunc001 - VALUE Attribute
!*                               (CHARACTER Length > 1)
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : April 25, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : User Defined Functions with Dummy Argument(s)
!*                               some of which specifiy the VALUE Attribute
!*  SECONDARY FUNCTIONS TESTED : Some Dummy Arguments are of Type Character
!*                               where the Length is greater than 1
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : VALUE Attribute
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  5.1 Type declaration statements
!*
!*  R501 type-declaration-stmt  is  declaration-type-spec [ [ , attr-spec ]&
!*                                      &... :: ] entity-decl -list
!*
!*  R503 attr-spec              is  access-spec
!*                              or  ALLOCATABLE
!*                              or  ASYNCHRONOUS
!*  ...
!*                              or  VALUE
!*  ...
!*
!*  C528 (R501) If the VALUE attribute is specified, the length type
!*              parameter values shall be omitted or specified by
!*              initialization expressions.
!*
!*
!*  5.1.2.15 VALUE attribute
!*
!*  The VALUE attribute specifies a type of argument association (12.4.1.2)
!*  for a dummy argument.
!*
!*
!*  5.2.14 VALUE statement
!*
!*  R547 value-stmt             is  VALUE [ :: ] dummy-arg-name-list
!*
!*  The VALUE statement specifies the VALUE attribute (5.1.2.15) for a list
!*  of dummy arguments.
!*
!*
!*  12.4.1.2 Actual arguments associated with dummy data objects
!*
!*  If the dummy argument has the VALUE attribute it becomes associated with
!*  a definable anonymous data object whose initial value is that of the
!*  actual argument. Subsequent changes to the value or definition status of
!*  the dummy argument do not affect the actual argument. 
!*
!*  NOTE 12.22
!*  Fortran argument association is usually similar to call by reference and
!*  call by value-result. If the VALUE attribute is specified, the effect is
!*  as if the actual argument is assigned to a temporary, and the temporary
!*  is then argument associated with the dummy argument. The actual mechanism
!*  by which this happens is determined by the processor.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mCharValues
    CHARACTER(len = 4) :: xExpected
    CHARACTER(len = 4) :: yExpected
    CHARACTER(len = 3) :: zExpected

    CHARACTER(len = 4) :: fourLen = 'abcd'
END MODULE mCharValues

PROGRAM valueAttrUserFunc001
    USE mCharValues

    INTERFACE
        CHARACTER(len = 4) FUNCTION y( arg1 )
            CHARACTER(len = 4), VALUE :: arg1
        END FUNCTION y

        CHARACTER(len = 4) FUNCTION z( arg1 )
            CHARACTER(len = 3), VALUE :: arg1
        END FUNCTION z
    END INTERFACE

    CHARACTER(len = 5) :: charResult


    xExpected = 'NdaM'
    yExpected = 'abcd'

    charResult = x( y( fourLen ) )
    if (charResult /= 'NA=BC') then
        write(0, *) "charResult = '", charResult, "' (Expected 'NA=BC')"
        call zzrc( 1_4 )
    end if


    xExpected = 'NYYM'
    yExpected = 'YZXY'
    zExpected = 'abc'

    charResult = x( y( z( fourLen ) ) )
    if (charResult /= 'NA=BC') then
        write(0, *) "charResult = '", charResult, "' (Expected 'NA=BC')"
        call zzrc( 2_4 )
    end if


    contains

        CHARACTER(len = 5) FUNCTION x( arg1 )
            CHARACTER(len = 4) :: arg1
            VALUE arg1

            if (arg1 /= xExpected) then
                write(0, *) "arg1 = '", arg1, "' (Expected '", xExpected, "')"
                call zzrc( 3_4 )
            end if

            arg1( 2: ) = 'ABC'
            x = arg1( 1:2 ) // '=' // arg1( 3:4 )

            if (fourLen /= 'abcd') then
                write(0, *) "fourLen = '", fourLen,&
                            "' (Expected '", xExpected, "')"
                call zzrc( 4_4 )
            end if

        END FUNCTION x

END PROGRAM valueAttrUserFunc001


CHARACTER(len = 4) FUNCTION y( arg1 )
    USE mCharValues

    CHARACTER(len = 4), VALUE :: arg1

    if (arg1 /= yExpected) then
        write(0, *) "arg1 = '", arg1, "' (Expected '", yExpected, "')"
        call zzrc( 5_4 )
    end if

    arg1( 2:3 ) = 'MN'
    y = arg1( 3:4 ) // arg1( 1:2 )

END FUNCTION y


CHARACTER(len = 4) FUNCTION z( arg1 )
    USE mCharValues

    CHARACTER(len = 3), VALUE :: arg1

    if (arg1 /= zExpected) then
        write(0, *) "arg1 = '", arg1, "' (Expected '", zExpected, "')"
        call zzrc( 6_4 )
    end if

    arg1 = 'XYZ'
    z = arg1( 2:3 ) // arg1( 1:2 )

END FUNCTION z
