! *********************************************************************
!* ===================================================================
!*
!* DATE                         : June 25, 2012
!*
!* PRIMARY FUNCTIONS TESTED     : 399982 - C Interop: Optional Argument
!* SECONDARY FUNTIONS TESTED    :
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  :
!*
!* Calling a BIND(C) procedure from Fortran, (where the procedure is defined in C)
!*
!* Actual Argument:
!*   When have multiple optional dummy arguments in a function or subroutine, supply
!*   with no actual argument, or actual arguments for all the optional ones, or
!*   actual arguments for some of the optional arguments
!*
!* Dummy Argument:
!*  all the interoperable C types from above table
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  12/06/14    YZ     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

use iso_c_binding
implicit none

interface

   subroutine c_func_int(arg1) bind(c)
    import
    integer(c_int), optional :: arg1
   end

   subroutine c_func_short(arg1) bind(c)
    import
    integer(c_short), optional :: arg1
   end

   subroutine c_func_long(arg1) bind(c)
    import
    integer(c_long), optional :: arg1
   end

   subroutine c_func_long_long(arg1) bind(c)
    import
    integer(c_long_long), optional :: arg1
   end

   subroutine c_func_unsigned(arg1) bind(c)
    import
    integer(c_signed_char), optional :: arg1
   end

   subroutine c_func_size_t(arg1) bind(c)
    import
    integer(c_size_t), optional :: arg1
   end

   subroutine c_func_int8_t(arg1) bind(c)
    import
    integer(c_int8_t), optional :: arg1
   end

   subroutine c_func_int16_t(arg1) bind(c)
    import
    integer(c_int16_t), optional :: arg1
   end

   subroutine c_func_int32_t(arg1) bind(c)
    import
    integer(c_int32_t), optional :: arg1
   end

   subroutine c_func_int64_t(arg1) bind(c)
    import
    integer(c_int64_t), optional :: arg1
   end

   subroutine c_func_least8_t(arg1) bind(c)
    import
    integer(c_int_least8_t), optional :: arg1
   end

   subroutine c_func_least16_t(arg1) bind(c)
    import
    integer(c_int_least16_t), optional :: arg1
   end

   subroutine c_func_least32_t(arg1) bind(c)
    import
    integer(c_int_least32_t), optional :: arg1
   end

   subroutine c_func_least64_t(arg1) bind(c)
    import
    integer(c_int_least64_t), optional :: arg1
   end

   subroutine c_func_fast8_t(arg1) bind(c)
    import
    integer(c_int_fast8_t), optional :: arg1
   end

   subroutine c_func_fast16_t(arg1) bind(c)
    import
    integer(c_int_fast16_t), optional :: arg1
   end

   subroutine c_func_fast32_t(arg1) bind(c)
    import
    integer(c_int_fast32_t), optional :: arg1
   end

   subroutine c_func_fast64_t(arg1) bind(c)
    import
    integer(c_int_fast64_t), optional :: arg1
   end

   subroutine c_func_intmax_t(arg1) bind(c)
    import
    integer(c_intmax_t), optional :: arg1
   end

   subroutine c_func_intptr_t(arg1) bind(c)
    import
    integer(c_intptr_t), optional :: arg1
   end

   subroutine c_func_float(arg1) bind(c)
    import
    real(c_float), optional :: arg1
   end

   subroutine c_func_double(arg1) bind(c)
    import
    real(c_double), optional :: arg1
   end

   subroutine c_func_long_double(arg1) bind(c)
    import
    real(c_long_double), optional :: arg1
   end

   subroutine c_func_float_complex(arg1) bind(c)
    import
    complex(c_float_complex), optional :: arg1
   end

   subroutine c_func_double_complex(arg1) bind(c)
    import
    complex(c_double_complex), optional :: arg1
   end

   subroutine c_func_long_double_complex(arg1) bind(c)
    import
    complex(c_long_double_complex), optional :: arg1
   end

   subroutine c_func_bool(arg1) bind(c)
    import
    logical(c_bool), optional :: arg1
   end

   subroutine c_func_char(arg1) bind(c)
    import
    character(c_char), optional :: arg1
   end

end interface

    integer(c_int) ::               i1
    integer(c_short) ::             i2
    integer(c_long) ::              i3
    integer(c_long_long) ::         i4
    integer(c_signed_char) ::       i5
    integer(c_size_t) ::            i6
    integer(c_int8_t) ::            i7
    integer(c_int16_t) ::           i8
    integer(c_int32_t) ::           i9
    integer(c_int64_t) ::           i10
    integer(c_int_least8_t) ::      i11
    integer(c_int_least16_t) ::     i12
    integer(c_int_least32_t) ::     i13
    integer(c_int_least64_t) ::     i14
    integer(c_int_fast8_t) ::       i15
    integer(c_int_fast16_t) ::      i16
    integer(c_int_fast32_t) ::      i17
    integer(c_int_fast64_t) ::      i18
    integer(c_intmax_t) ::          i19
    integer(c_intptr_t) ::          i20
    real(c_float) ::                r1
    real(c_double) ::               r2
    real(c_long_double) ::          r3
    complex(c_float_complex) ::     c1
    complex(c_double_complex) ::    c2
    complex(c_long_double_complex)::        c3
    logical(c_bool) ::              ll
    character(c_char) ::            cc

    i1=1
    i2=2
    i3=3
    i4=4_C_LONG_LONG
    i5=5
    i6=6
    i7=7
    i8=8
    i9=9
    i10=10
    i11=11
    i12=12
    i13=13
    i14=14
    i15=15
    i16=16
    i17=17
    i18=18
    i19=19
    i20=20
    r1=3.0
    r2=4.0
    r3=5.0
    c1=(5.0e0,5.0e0)
    c2=(10.0d0,10.0d0)
    c3=(15.0d0,15.0d0)
    ll=.true.
    cc='a'

    call c_func_int()
    call c_func_short()
    call c_func_long()
    call c_func_long_long()
    call c_func_unsigned()
    call c_func_size_t()
    call c_func_int8_t()
    call c_func_int16_t()
    call c_func_int32_t()
    call c_func_int64_t()
    call c_func_least8_t()
    call c_func_least16_t()
    call c_func_least32_t()
    call c_func_least64_t()
    call c_func_fast8_t()
    call c_func_fast16_t()
    call c_func_fast32_t()
    call c_func_fast64_t()
    call c_func_intmax_t()
    call c_func_intptr_t()
    call c_func_float()
    call c_func_double()
    call c_func_long_double()
    call c_func_float_complex()
    call c_func_double_complex()
    call c_func_long_double_complex()
    call c_func_bool()
    call c_func_char()

    call c_func_int(i1)
    call c_func_short(i2)
    call c_func_long(i3)
    call c_func_long_long(i4)
    call c_func_unsigned(i5)
    call c_func_size_t(i6)
    call c_func_int8_t(i7)
    call c_func_int16_t(i8)
    call c_func_int32_t(i9)
    call c_func_int64_t(i10)
    call c_func_least8_t(i11)
    call c_func_least16_t(i12)
    call c_func_least32_t(i13)
    call c_func_least64_t(i14)
    call c_func_fast8_t(i15)
    call c_func_fast16_t(i16)
    call c_func_fast32_t(i17)
    call c_func_fast64_t(i18)
    call c_func_intmax_t(i19)
    call c_func_intptr_t(i20)
    call c_func_float(r1)
    call c_func_double(r2)
    call c_func_long_double(r3)
    call c_func_float_complex(c1)
    call c_func_double_complex(c2)
    call c_func_long_double_complex(c3)
    call c_func_bool(ll)
    call c_func_char(cc)

end


