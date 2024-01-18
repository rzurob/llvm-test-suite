!******************************************************************************
!*  ===========================================================================
!*
!*  TEST CASE NAME  : F2008/value/pure/unit_tests/purevalue09.f
!*  DATE            : 2010-12-01
!*  DRIVER STANZA   : xlf2003
!*
!*  DESCRIPTION
!*  - C main calls Fortran pure procedures, passing all interoperable intrinsic
!*    types, declared with value attribute
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

integer(C_INT) pure function foo ( &
        dc_signed_char, &
        dc_short, &
        dc_int, &
        dc_long, &
        dc_long_long, &
        dc_size_t, &
        dc_intptr_t, &
        dc_intmax_t, &
        dc_int8_t, &
        dc_int16_t, &
        dc_int32_t, &
        dc_int64_t, &
        dc_int_least8_t, &
        dc_int_least16_t, &
        dc_int_least32_t, &
        dc_int_least64_t, &
        dc_int_fast8_t, &
        dc_int_fast16_t, &
        dc_int_fast32_t, &
        dc_int_fast64_t, &
        dc_float, &
        dc_double, &
        dc_long_double, &
        dc_float_Complex, &
        dc_double_Complex, &
        dc_long_double_complex, &
        dc__Bool, &
        dc_char )

    use, intrinsic :: ISO_C_BINDING
    use pcheck_prec

    integer(C_SIGNED_CHAR), value :: dc_signed_char       ! kind=1
    integer(C_SHORT), value :: dc_short                   ! kind=2
    integer(C_INT), value :: dc_int                       ! kind=4
    integer(C_LONG), value :: dc_long                     ! kind=4 with -q32,
                                                          ! kind=8 with -q64
    integer(C_LONG_LONG), value :: dc_long_long           ! kine=8
    integer(C_SIZE_T), value :: dc_size_t                 ! kind=4 with -q32,
                                                          ! kind=8 with -q64

    integer(C_INTPTR_T), value :: dc_intptr_t             ! kind=4 with -q32,
                                                          ! kind=8 with -q64
    integer(C_INTMAX_T), value :: dc_intmax_t             ! kind=8
    integer(C_INT8_T), value :: dc_int8_t                 ! kind=1
    integer(C_INT16_T), value :: dc_int16_t               ! kind=2
    integer(C_INT32_T), value :: dc_int32_t               ! kind=4
    integer(C_INT64_T), value :: dc_int64_t               ! kind=8

    integer(C_INT_LEAST8_T), value :: dc_int_least8_t     ! kind=1
    integer(C_INT_LEAST16_T), value :: dc_int_least16_t   ! kind=2
    integer(C_INT_LEAST32_T), value :: dc_int_least32_t   ! kind=4
    integer(C_INT_LEAST64_T), value :: dc_int_least64_t   ! kind=8

    integer(C_INT_FAST8_T), value :: dc_int_fast8_t       ! kind=1
    integer(C_INT_FAST16_T), value :: dc_int_fast16_t     ! kind=4
    integer(C_INT_FAST32_T), value :: dc_int_fast32_t     ! kind=4
    integer(C_INT_FAST64_T), value :: dc_int_fast64_t     ! kind=8

    real(C_FLOAT), value :: dc_float                      ! kind=4
    real(C_DOUBLE), value :: dc_double                    ! kind=8
    real(C_LONG_DOUBLE), value :: dc_long_double          ! kind=16
    complex(C_FLOAT_COMPLEX), value :: dc_float_Complex   ! kind=4
    complex(C_DOUBLE_COMPLEX), value :: dc_double_Complex ! kind=8
    complex(C_LONG_DOUBLE_COMPLEX), value :: dc_long_double_complex  ! kind=16

    logical(C_BOOL), value :: dc__Bool                    ! kind=1
    character(C_CHAR), value :: dc_char                   ! kind=1

    integer :: tmp
    tmp = 1

    if (dc_signed_char /= ichar('a')) tmp = tmp/0
    if (dc_short /= x'1234') tmp = tmp/0
    if (dc_int /= x'567890ab') tmp = tmp/0

#ifdef __64BIT__
    if (dc_long /= x'cdef012345678901') tmp = tmp/0
    if (dc_size_t /= x'234567890abcdef0') tmp = tmp/0
    if (dc_intptr_t /= x'1234567890abcdef') tmp = tmp/0
#else
    if (dc_long /= x'cdef0123') tmp = tmp/0
    if (dc_size_t /= x'4567890a') tmp = tmp/0
    if (dc_intptr_t /= x'bcdef012') tmp = tmp/0
#endif

    if (dc_long_long /= x'01234567890abcde') tmp = tmp/0
    if (dc_intmax_t /= x'f01234567890abcd') tmp = tmp/0
    if (dc_int8_t /= x'6f') tmp = tmp/0
    if (dc_int16_t /= x'0123') tmp = tmp/0
    if (dc_int32_t /= x'45678901') tmp = tmp/0
    if (dc_int64_t /= x'2345678901234567') tmp = tmp/0
    if (dc_int_least8_t /= x'59') tmp = tmp/0
    if (dc_int_least16_t /= x'7bcd') tmp = tmp/0
    if (dc_int_least32_t /= x'12345678') tmp = tmp/0
    if (dc_int_least64_t /= x'90abcdef01234567') tmp = tmp/0
    if (dc_int_fast8_t /= x'34') tmp = tmp/0
    if (dc_int_fast16_t /= x'789a') tmp = tmp/0
    if (dc_int_fast32_t /= x'890abcde') tmp = tmp/0
    if (dc_int_fast64_t /= x'4567890123456789') tmp = tmp/0

    if (.not. (prec(dc_float, 1.2_4/3.4_4))) tmp = tmp/0
    if (.not. (prec(dc_double, 5.6_8/7.8_8))) tmp = tmp/0
    if (.not. (prec(dc_long_double,9.1_16/11.12_16))) tmp = tmp/0
    if (.not. (prec(dc_float_Complex,(13.14_4,15.16_4)))) tmp = tmp/0
    if (.not. (prec(dc_double_Complex,(17.18_8,19.20_8)))) tmp = tmp/0
    if (.not. (prec(dc_long_double_Complex,(21.22_16,23.24_16)))) tmp = tmp/0

    if (.not. dc__Bool) tmp = tmp/0
    if (dc_char /= 'c') tmp = tmp/0

    dc_signed_char = 0
    dc_short = 0
    dc_int = 0
    dc_long = 0
    dc_long_long = 0
    dc_size_t = 0
    dc_intptr_t = 0
    dc_intmax_t = 0
    dc_int8_t = 0
    dc_int16_t = 0
    dc_int32_t = 0
    dc_int64_t = 0
    dc_int_least8_t = 0
    dc_int_least16_t = 0
    dc_int_least32_t = 0
    dc_int_least64_t = 0
    dc_int_fast8_t = 0
    dc_int_fast16_t = 0
    dc_int_fast32_t = 0
    dc_int_fast64_t = 0
    dc_float = 0
    dc_double = 0
    dc_long_double = 0
    dc_float_Complex = (0.0,0.0)
    dc_double_Complex = (0.0,0.0)
    dc_long_double_complex = (0.0,0.0)
    dc__Bool = .false.
    dc_char = ''

    foo = 12345
end function foo

!-------------------------------------------------------------------

pure subroutine sub ( &
        dc_int_result, &
        dc_short, &
        dc_int, &
        dc_long, &
        dc_long_long, &
        dc_size_t, &
        dc_intptr_t, &
        dc_intmax_t, &
        dc_int8_t, &
        dc_int16_t, &
        dc_int32_t, &
        dc_int64_t, &
        dc_int_least8_t, &
        dc_int_least16_t, &
        dc_int_least32_t, &
        dc_int_least64_t, &
        dc_int_fast8_t, &
        dc_int_fast16_t, &
        dc_int_fast32_t, &
        dc_int_fast64_t, &
        dc_float, &
        dc_double, &
        dc_long_double, &
        dc_float_Complex, &
        dc_double_Complex, &
        dc_long_double_complex, &
        dc__Bool, &
        dc_char )

    use, intrinsic :: ISO_C_BINDING
    use pcheck_prec

    integer(C_INT), intent(out) :: dc_int_result
    integer(C_SHORT), value :: dc_short                   ! kind=2
    integer(C_INT), value :: dc_int                       ! kind=4
    integer(C_LONG), value :: dc_long                     ! kind=4 with -q32,
                                                          ! kind=8 with -q64
    integer(C_LONG_LONG), value :: dc_long_long           ! kine=8
    integer(C_SIZE_T), value :: dc_size_t                 ! kind=4 with -q32,
                                                          ! kind=8 with -q64

    integer(C_INTPTR_T), value :: dc_intptr_t             ! kind=4 with -q32,
                                                          ! kind=8 with -q64
    integer(C_INTMAX_T), value :: dc_intmax_t             ! kind=8
    integer(C_INT8_T), value :: dc_int8_t                 ! kind=1
    integer(C_INT16_T), value :: dc_int16_t               ! kind=2
    integer(C_INT32_T), value :: dc_int32_t               ! kind=4
    integer(C_INT64_T), value :: dc_int64_t               ! kind=8

    integer(C_INT_LEAST8_T), value :: dc_int_least8_t     ! kind=1
    integer(C_INT_LEAST16_T), value :: dc_int_least16_t   ! kind=2
    integer(C_INT_LEAST32_T), value :: dc_int_least32_t   ! kind=4
    integer(C_INT_LEAST64_T), value :: dc_int_least64_t   ! kind=8

    integer(C_INT_FAST8_T), value :: dc_int_fast8_t       ! kind=1
    integer(C_INT_FAST16_T), value :: dc_int_fast16_t     ! kind=4
    integer(C_INT_FAST32_T), value :: dc_int_fast32_t     ! kind=4
    integer(C_INT_FAST64_T), value :: dc_int_fast64_t     ! kind=8

    real(C_FLOAT), value :: dc_float                      ! kind=4
    real(C_DOUBLE), value :: dc_double                    ! kind=8
    real(C_LONG_DOUBLE), value :: dc_long_double          ! kind=16
    complex(C_FLOAT_COMPLEX), value :: dc_float_Complex   ! kind=4
    complex(C_DOUBLE_COMPLEX), value :: dc_double_Complex ! kind=8
    complex(C_LONG_DOUBLE_COMPLEX), value :: dc_long_double_complex  ! kind=16

    logical(C_BOOL), value :: dc__Bool                    ! kind=1
    character(C_CHAR), value :: dc_char                   ! kind=1

    integer :: tmp
    tmp = 1

    if (dc_short /= x'1234') tmp = tmp/0
    if (dc_int /= x'567890ab') tmp = tmp/0

#ifdef __64BIT__
    if (dc_long /= x'cdef012345678901') tmp = tmp/0
    if (dc_size_t /= x'234567890abcdef0') tmp = tmp/0
    if (dc_intptr_t /= x'1234567890abcdef') tmp = tmp/0
#else
    if (dc_long /= x'cdef0123') tmp = tmp/0
    if (dc_size_t /= x'4567890a') tmp = tmp/0
    if (dc_intptr_t /= x'bcdef012') tmp = tmp/0
#endif

    if (dc_long_long /= x'01234567890abcde') tmp = tmp/0
    if (dc_intmax_t /= x'f01234567890abcd') tmp = tmp/0
    if (dc_int8_t /= x'6f') tmp = tmp/0
    if (dc_int16_t /= x'0123') tmp = tmp/0
    if (dc_int32_t /= x'45678901') tmp = tmp/0
    if (dc_int64_t /= x'2345678901234567') tmp = tmp/0
    if (dc_int_least8_t /= x'59') tmp = tmp/0
    if (dc_int_least16_t /= x'7bcd') tmp = tmp/0
    if (dc_int_least32_t /= x'12345678') tmp = tmp/0
    if (dc_int_least64_t /= x'90abcdef01234567') tmp = tmp/0
    if (dc_int_fast8_t /= x'34') tmp = tmp/0
    if (dc_int_fast16_t /= x'789a') tmp = tmp/0
    if (dc_int_fast32_t /= x'890abcde') tmp = tmp/0
    if (dc_int_fast64_t /= x'4567890123456789') tmp = tmp/0

    if (.not. (prec(dc_float, 1.2_4/3.4_4))) tmp = tmp/0
    if (.not. (prec(dc_double, 5.6_8/7.8_8))) tmp = tmp/0
    if (.not. (prec(dc_long_double,9.1_16/11.12_16))) tmp = tmp/0
    if (.not. (prec(dc_float_Complex,(13.14_4,15.16_4)))) tmp = tmp/0
    if (.not. (prec(dc_double_Complex,(17.18_8,19.20_8)))) tmp = tmp/0
    if (.not. (prec(dc_long_double_Complex,(21.22_16,23.24_16)))) tmp = tmp/0

    if (.not. dc__Bool) tmp = tmp/0
    if (dc_char /= 'c') tmp = tmp/0

    dc_short = 0
    dc_int = 0
    dc_long = 0
    dc_long_long = 0
    dc_size_t = 0
    dc_intptr_t = 0
    dc_intmax_t = 0
    dc_int8_t = 0
    dc_int16_t = 0
    dc_int32_t = 0
    dc_int64_t = 0
    dc_int_least8_t = 0
    dc_int_least16_t = 0
    dc_int_least32_t = 0
    dc_int_least64_t = 0
    dc_int_fast8_t = 0
    dc_int_fast16_t = 0
    dc_int_fast32_t = 0
    dc_int_fast64_t = 0
    dc_float = 0
    dc_double = 0
    dc_long_double = 0
    dc_float_Complex = (0.0,0.0)
    dc_double_Complex = (0.0,0.0)
    dc_long_double_complex = (0.0,0.0)
    dc__Bool = .false.
    dc_char = ''

    dc_int_result = 12345
end subroutine sub

