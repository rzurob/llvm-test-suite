!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : valueAttrUserFunc003 - VALUE Attribute
!*                               (CHARACTER Length > 1)
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : April 26, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : User Defined Functions with Dummy Argument(s)
!*                               that specifiy the VALUE Attribute
!*  SECONDARY FUNCTIONS TESTED : RECURSIVE Function
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

MODULE m
    CHARACTER(LEN = 7) :: check
END MODULE m


PROGRAM valueAttrUserFunc003
    USE m

    INTERFACE
        RECURSIVE INTEGER FUNCTION iFact( n )
            INTEGER :: n
        END FUNCTION iFact

        RECURSIVE CHARACTER(LEN = 7) FUNCTION fact( n )
            CHARACTER(LEN = 7), VALUE :: n
        END FUNCTION fact
    END INTERFACE

    CHARACTER(LEN = 7) :: num
    CHARACTER(LEN = 7) :: charFact
    CHARACTER(LEN = 7) :: factCheck


    DO i = 1, 10
        WRITE(num, 100) i
        check = num

        j = iFact( i )
        WRITE(factCheck, 100) j

        charFact = fact( num )
        if (charFact /= factCheck) then
            write(0, *) "iFact(", i, ") =", j
            write(0, *) "num = '", num, "' (should be '", factCheck, "')"
            call zzrc( 1_4 )
        end if

        READ(num, 100) k
        if (k /= i) then
            write(0, *) "num =", num, "(", k, ") should be", i
            call zzrc( 2_4 )
        end if

        write(6, 200) i, j, num, charFact
200     FORMAT("iFact(",I2,") = '",I7,"', fact(",A7,") = '",A7,"'")
    END DO

100 FORMAT(I7)

END PROGRAM valueAttrUserFunc003


RECURSIVE INTEGER FUNCTION iFact( n )
    INTEGER :: n

    if (n == 1) then
        iFact = 1
    else
        iFact = iFact( (n - 1) ) * n
    end if

END FUNCTION iFact


RECURSIVE CHARACTER(LEN = 7) FUNCTION fact( n )
    USE m

    CHARACTER(LEN = 7), VALUE :: n

    CHARACTER(LEN = 7) :: o
    CHARACTER(LEN = 7) :: p

100 FORMAT(I7)


    if (n /= check) then
        write(0, *) "n = '", n, "' (should be '", check, "')"
        call zzrc( 3_4 )
    end if


    READ(n, 100) i
    if (i == 1) then
        WRITE(n, 100) 1

    else
        WRITE(o, 100) (i - 1)
        check = o

        WRITE(n, 100) (i - 1)
        p = fact( n )

        if (n /= o) then
            write(0, *) "n = '", n, "' (should be '", o, "')"
            call zzrc( 4_4 )
        end if

        READ(p, 100) j
        WRITE(n, 100) (i * j)
    end if

    fact = n

END FUNCTION fact
