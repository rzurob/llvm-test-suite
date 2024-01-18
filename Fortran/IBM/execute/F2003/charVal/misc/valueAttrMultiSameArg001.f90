!*  ===================================================================
!*
!*                               (CHARACTER Length > 1)
!*
!*  DATE                       : April 25, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Multiple Dummy Arguments of Type Character
!*                               of Length > 1 with the VALUE Attribute
!*  SECONDARY FUNCTIONS TESTED : The Actual Arguments are the same Variable
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : VALUE Attribute
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
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
!*  5.1.2.15 VALUE attribute
!*
!*  The VALUE attribute specifies a type of argument association (12.4.1.2)
!*  for a dummy argument.
!*
!*  5.2.14 VALUE statement
!*
!*  R547 value-stmt             is  VALUE [ :: ] dummy-arg-name-list
!*
!*  The VALUE statement specifies the VALUE attribute (5.1.2.15) for a list
!*  of dummy arguments.
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

MODULE m

    CONTAINS

        CHARACTER(len = 4) FUNCTION x(arg1, arg2)
            CHARACTER(len = 4), VALUE :: arg1
            CHARACTER(len = 4), VALUE :: arg2

            if (arg1 /= 'abcd') then
                WRITE(0, *) "arg1 = '", arg1, "' (Expected 'abcd')"
                CALL zzrc( 1_4 )
            end if

            arg1 = 'zyxw'

            if (arg2 /= 'abcd') then
                WRITE(0, *) "arg2 = '", arg2, "' (Expected 'abcd')"
                CALL zzrc( 2_4 )
            end if

            x = arg1( 1:2 ) // arg2( 3:4 )

        END FUNCTION x
END MODULE m


PROGRAM valueAttrMultiSameArg001
    USE m

    INTERFACE
        SUBROUTINE y(arg1, arg2)
            CHARACTER(len = 4), VALUE :: arg1
            CHARACTER(len = 4), VALUE :: arg2
        END SUBROUTINE y

        CHARACTER(len = 4) FUNCTION z(arg1, arg2, arg3)
            CHARACTER(len = 4), VALUE :: arg1
            CHARACTER(len = 4), VALUE :: arg2
            CHARACTER(len = 4), VALUE :: arg3
        END FUNCTION z
    END INTERFACE

    CHARACTER(len = 4) :: charResult
    CHARACTER(len = 4) :: charValue = 'abcd'


    charResult = x(charValue, charValue)
    if (charResult /= 'zycd') then
        CALL zzrc( 3_4 )

    else if (charValue /= 'abcd') then
        CALL zzrc( 4_4 )
    end if

    call y(charValue, charValue)
    if (charValue /= 'abcd') then
        CALL zzrc( 5_4 )
    end if

    charResult = z(charValue, charValue, charValue)
    if (charResult /= 'ycbc') then
        CALL zzrc( 6_4 )

    else if (charValue /= 'abcd') then
        CALL zzrc( 7_4 )
    end if

END PROGRAM valueAttrMultiSameArg001


SUBROUTINE y(arg1, arg2)
    USE m

    CHARACTER(len = 4), VALUE :: arg1
    CHARACTER(len = 4), VALUE :: arg2


    if (arg1 /= 'abcd') then
        CALL zzrc( 8_4 )
    end if

    arg1 = x(arg2, arg2)
    if (arg1 /= 'zycd') then
        CALL zzrc( 9_4 )

    else if (arg2 /= 'abcd') then
        CALL zzrc( 10_4 )
    end if

END SUBROUTINE y


CHARACTER(len = 4) FUNCTION z(arg1, arg2, arg3)
    USE m

    CHARACTER(len = 4), VALUE :: arg1
    CHARACTER(len = 4), VALUE :: arg2
    CHARACTER(len = 4), VALUE :: arg3


    if (arg2 /= 'abcd') then
        CALL zzrc( 11_4 )
    end if

    arg2 = x(arg2, arg2)
    if (arg1 /= 'abcd') then
        CALL zzrc( 12_4 )

    else if (arg3 /= 'abcd') then
        CALL zzrc( 13_4 )
    end if

    z = arg2( 2:3 ) // arg1( 2:3 )

END FUNCTION z
