!*  ===================================================================
!*
!*                               (CHARACTER Length > 1)
!*
!*  DATE                       : April 26, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : User Defined Functions with Dummy Argument(s)
!*                               some of which specifiy the VALUE Attribute
!*  SECONDARY FUNCTIONS TESTED : Some Dummy Arguments are of Type Character
!*                               where the Length is greater than 1
!*
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
    CHARACTER(LEN = 4) :: xArgIn
    CHARACTER(LEN = 4) :: xPected

    CHARACTER(LEN = 4) :: yArgIn
    CHARACTER(LEN = 4) :: yPected

    CHARACTER(LEN = 8) :: zResult
    CHARACTER(LEN = 8) :: zPected
END MODULE m


PROGRAM valueAttrUserFunc002
    USE m

    INTERFACE
        CHARACTER(LEN = 4) FUNCTION x( arg1 )
            character(len = 4), value :: arg1
        END FUNCTION x

        CHARACTER(LEN = 4) FUNCTION y( arg1 )
            character(len = 4), value :: arg1
        END FUNCTION y

        CHARACTER(LEN = 8) FUNCTION z( arg1 )
            character(len = 8), value :: arg1
        END FUNCTION z
    END INTERFACE


    xArgIn  = 'abcd'
    xPected = 'abcd'

    yArgIn  = 'efgh'
    yPected = 'efgh'

    zPected = 'aYXdeUTh'


    zResult = z( (x( xArgIn ) // y( yArgIn )) )
    if (zResult /= 'ZYXWVUTS') then
        call zzrc( 1_4 )
    end if

END PROGRAM valueAttrUserFunc002


CHARACTER(LEN = 4) FUNCTION x( arg1 )
    USE m

    character(len = 4), value :: arg1

    if (arg1 /= xPected) then
        call zzrc( 2_4 )
    end if

    arg1( 2:3 ) = 'YX'
    x = arg1

    if (xArgIn /= xPected) then
        call zzrc( 3_4 )
    end if

END FUNCTION x


CHARACTER(LEN = 4) FUNCTION y( arg1 )
    USE m

    character(len = 4), value :: arg1

    if (arg1 /= yPected) then
        call zzrc( 4_4 )
    end if

    arg1( 3:3 ) = 'T'
    arg1( 2:2 ) = 'U'

    y = arg1

    if (yArgIn /= yPected) then
        call zzrc( 5_4 )
    end if

END FUNCTION y


CHARACTER(LEN = 8) FUNCTION z( arg1 )
    USE m

    character(len = 8), value :: arg1

    if (arg1 /= zPected) then
        call zzrc( 6_4 )
    end if

    arg1( :1 )  = 'Z'
    arg1( 4:5 ) = 'WV'
    arg1( 8: )  = 'S'

    z = arg1

END FUNCTION z
