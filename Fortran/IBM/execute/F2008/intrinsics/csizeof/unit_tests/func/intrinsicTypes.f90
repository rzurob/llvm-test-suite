program t
    use, intrinsic :: iso_c_binding, renamed_c_sizeof => c_sizeof
    implicit none

    integer   di
    real      dr
    complex   dc
    character dch

    integer(c_int)            i1!4
    integer(c_short)          i2!2
    integer(c_long)           i3!4 or 8(64bit)
    integer(c_long_long)      i4!8

    integer(c_signed_char)    i5!1
    integer(c_size_t)         i6!==c_long !4 or 8(64bit)

    integer(c_int8_t)         i7!==c_signed_char 1
    integer(c_int16_t)        i8!==c_short 2
    integer(c_int32_t)        i9!4
    integer(c_int64_t)        i10!8

    integer(c_int_least8_t)   i11!1
    integer(c_int_least16_t)  i12!2
    integer(c_int_least32_t)  i13!4
    integer(c_int_least64_t)  i14!8

    integer(c_int_fast8_t)    i15!1
    integer(c_int_fast16_t)   i16!if__linux__ == c_long 4 or 8(64bit)  else 2
    integer(c_int_fast32_t)   i17!if__linux__ == c_long 4 or 8(64bit)
    integer(c_int_fast64_t)   i18!8

    integer(c_intmax_t)       i19!8
    integer(c_intptr_t)       i20!==c_long !4 or 8(64bit)

    real(c_float)             r1!4
    real(c_double)            r2!8
    real(c_long_double)       r3!16

    complex(c_float_complex)       c1!8
    complex(c_double_complex)      c2!16
    complex(c_long_double_complex) c3!32

    logical(c_bool)                ll!1
    character(c_char)              cc!1

    if (renamed_c_sizeof(di) /= kind(di)) error stop 1
    if (renamed_c_sizeof(dr) /= kind(dr)) error stop 2
    if (renamed_c_sizeof(dc) /= 2 * kind(dc)) error stop 3
    if (renamed_c_sizeof(dch) /= kind(dch)) error stop 4

    if (renamed_c_sizeof(i1) /= c_int) error stop 5
    if (renamed_c_sizeof(i2) /= c_short) error stop 6
    if (renamed_c_sizeof(i3) /= c_long) error stop 7
    if (renamed_c_sizeof(i4) /= c_long_long) error stop 8

    if (renamed_c_sizeof(i5) /= c_signed_char) error stop 9
    if (renamed_c_sizeof(i6) /= c_size_t) error stop 10

    if (renamed_c_sizeof(i7) /= c_int8_t) error stop 11
    if (renamed_c_sizeof(i8) /= c_int16_t) error stop 12
    if (renamed_c_sizeof(i9) /= c_int32_t) error stop 13
    if (renamed_c_sizeof(i10) /= c_int64_t) error stop 14

    if (renamed_c_sizeof(i11) /= c_int_least8_t) error stop 15
    if (renamed_c_sizeof(i12) /= c_int_least16_t) error stop 16
    if (renamed_c_sizeof(i13) /= c_int_least32_t) error stop 17
    if (renamed_c_sizeof(i14) /= c_int_least64_t) error stop 18

    if (renamed_c_sizeof(i15) /= c_int_fast8_t) error stop 19
    if (renamed_c_sizeof(i16) /= c_int_fast16_t) error stop 20
    if (renamed_c_sizeof(i17) /= c_int_fast32_t) error stop 21
    if (renamed_c_sizeof(i18) /= c_int_fast64_t) error stop 22

    if (renamed_c_sizeof(i19) /= c_intmax_t) error stop 23
    if (renamed_c_sizeof(i20) /= c_intptr_t) error stop 24

    if (renamed_c_sizeof(r1) /= c_float) error stop 25
    if (renamed_c_sizeof(r2) /= c_double) error stop 26
    if (renamed_c_sizeof(r3) /= c_long_double) error stop 27

    if (renamed_c_sizeof(c1) /= 2 * c_float_complex) error stop 28
    if (renamed_c_sizeof(c2) /= 2 * c_double_complex) error stop 29
    if (renamed_c_sizeof(c3) /= 2 * c_long_double_complex) error stop 30

    if (renamed_c_sizeof(ll) /= c_bool) error stop 31
    if (renamed_c_sizeof(cc) /= c_char) error stop 32
end program
