!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 13, 2012
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Calling a BIND(C) procedure from Fortran
!*                               where the procedure is defined in C
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890

use, intrinsic :: iso_c_binding
use iso_fortran_env
implicit none
integer(c_short)               :: i_short
integer(c_int)                 :: i_int
integer(c_long)                :: i_long
integer(c_long_long)           :: i_long_long
integer(c_signed_char)         :: i_signed_char
integer(c_size_t)              :: i_size_t
integer(c_int8_t)              :: i_int8_t
integer(c_int16_t)             :: i_int16_t
integer(c_int32_t)             :: i_int32_t
integer(c_int64_t)             :: i_int64_t
integer(c_int_least8_t)        :: i_int_least8_t
integer(c_int_least16_t)       :: i_int_least16_t
integer(c_int_least32_t)       :: i_int_least32_t
integer(c_int_least64_t)       :: i_int_least64_t
integer(c_int_fast8_t)         :: i_int_fast8_t
integer(c_int_fast16_t)        :: i_int_fast16_t
integer(c_int_fast32_t)        :: i_int_fast32_t
integer(c_int_fast64_t)        :: i_int_fast64_t
integer(c_intmax_t)            :: i_intmax_t
integer(c_intptr_t)            :: i_intptr_t
real(c_float)                  :: r_float
real(c_double)                 :: r_double
real(c_long_double)            :: r_long_double
complex(c_float_complex)       :: z_float_complex
complex(c_double_complex)      :: z_double_complex
complex(c_long_double_complex) :: z_long_double_complex
logical(c_bool)                :: l_bool
character(c_char)              :: x_char


interface
   subroutine c_sub(a, flag) BIND(c)
      use, intrinsic :: iso_c_binding
      implicit none
      type(*) :: a
      integer(c_int), value :: flag
   end subroutine c_sub
end interface

i_int = 4
i_long = 4
i_short = 2
i_size_t = 8
i_long_long = 8
i_signed_char = -127
i_int8_t = 23567
i_int16_t = i_int8_t*2
i_int32_t = i_int8_t*4
i_int64_t = i_int8_t*32
i_int_least8_t = 8
i_int_least16_t = 16
i_int_least32_t = 32
i_int_least64_t = 64
i_int_fast8_t = 8
i_int_fast16_t = 16
i_int_fast32_t = 32
i_int_fast64_t = 64
i_intmax_t = 100
i_intptr_t = 20
r_float = 1.03
r_double = 9.0025
r_long_double = 0.60d-10
z_float_complex = (5.0e0,5.0e0)
z_double_complex = (10.0d0,10.0d0)
z_long_double_complex = (15.0d0,15.0d0)
l_bool = .true.
x_char = 'A'


!---- testing unsigned short

!---- testing short
call c_sub(i_short, 1)
print*, "The actual value being:", i_short
flush(output_unit)

i_short = 32767
call c_sub(i_short, 1)
print*, "The actual value being:", i_short
flush(output_unit)

i_short = -32767
call c_sub(i_short, 1)
print*, "The actual value being:", i_short
flush(output_unit)

!---- testing int
call c_sub(i_int, 2)
print*, "The actual value being:", i_int
flush(output_unit)

i_int = 32767
call c_sub(i_int, 2)
print*, "The actual value being:", i_int
flush(output_unit)

i_int = -32767
call c_sub(i_int, 2)
print*, "The actual value being:", i_int
flush(output_unit)

!---- testing long
call c_sub(i_long, 3)
print*, "The actual value being:", i_long
flush(output_unit)

i_long = -2147483647
call c_sub(i_long, 3)
print*, "The actual value being:", i_long
flush(output_unit)

i_long = 2147483647
call c_sub(i_long, 3)
print*, "The actual value being:", i_long
flush(output_unit)

!---- testing long long
call c_sub(i_long_long, 4)
print*, "The actual value being:", i_long_long
flush(output_unit)

i_long_long = 1.2E+08
call c_sub(i_long_long, 4)
print*, "The actual value being:", i_long_long
flush(output_unit)

i_long_long = 2049.30042D-5
call c_sub(i_long_long, 4)
print*, "The actual value being:", i_long_long
flush(output_unit)

i_long_long = -2147483647
call c_sub(i_long_long, 4)
print*, "The actual value being:", i_long_long
flush(output_unit)

i_long_long = 2147483647
call c_sub(i_long_long, 4)
print*, "The actual value being:", i_long_long
flush(output_unit)

!---- testing c_signed_char
call c_sub(i_signed_char, 5)
print*, "The actual value being:", i_signed_char
flush(output_unit)

i_signed_char = 0
call c_sub(i_signed_char, 5)
print*, "The actual value being:", i_signed_char
flush(output_unit)

i_signed_char = 127
call c_sub(i_signed_char, 5)
print*, "The actual value being:", i_signed_char
flush(output_unit)


!---- testing c_int8_t
call c_sub(i_int8_t, 6)
print*, "The actual value being:", i_int8_t
flush(output_unit)

!---- testing c_int16_t
call c_sub(i_int16_t, 7)
print*, "The actual value being:", i_int16_t
flush(output_unit)

!---- testing c_int32_t
call c_sub(i_int32_t, 8)
print*, "The actual value being:", i_int32_t
flush(output_unit)

!---- testing c_int64_t
call c_sub(i_int64_t, 9)
print*, "The actual value being:", i_int64_t
flush(output_unit)

!---- testing c_int_least8_t
call c_sub(i_int_least8_t, 10)
print*, "The actual value being:", i_int_least8_t
flush(output_unit)

!---- testing c_int_least16_t
call c_sub(i_int_least16_t, 11)
print*, "The actual value being:", i_int_least16_t
flush(output_unit)

!---- testing c_int_least32_t
call c_sub(i_int_least32_t, 12)
print*, "The actual value being:", i_int_least32_t
flush(output_unit)

!---- testing c_int_least64_t
call c_sub(i_int_least64_t, 13)
print*, "The actual value being:", i_int_least64_t
flush(output_unit)

!---- testing c_int_fast8_t
call c_sub(i_int_fast8_t, 14)
print*, "The actual value being:", i_int_fast8_t
flush(output_unit)

!---- testing c_int_fast16_t
call c_sub(i_int_fast16_t, 15)
print*, "The actual value being:", i_int_fast16_t
flush(output_unit)

!---- testing c_int_fast32_t
call c_sub(i_int_fast32_t, 16)
print*, "The actual value being:", i_int_fast32_t
flush(output_unit)

!---- testing c_int_fast64_t
call c_sub(i_int_fast64_t, 17)
print*, "The actual value being:", i_int_fast64_t
flush(output_unit)

!---- testing c_intmax_t
call c_sub(i_intmax_t, 18)
print*, "The actual value being:", i_intmax_t
flush(output_unit)

!---- testing c_intptr_t
call c_sub(i_intptr_t, 19)
print*, "The actual value being:", i_intptr_t
flush(output_unit)

!---- testing c_float
call c_sub(r_float, 20)
print*, "The actual value being:", r_float
flush(output_unit)

!---- testing c_double
call c_sub(r_double, 21)
print*, "The actual value being:", r_double
flush(output_unit)

!---- testing c_long_double
call c_sub(r_long_double, 22)
print*, "The actual value being:", r_long_double
flush(output_unit)

!---- testing c_float_complex
call c_sub(z_float_complex, 23)
print*, "The actual value being:", z_float_complex
flush(output_unit)

!---- testing c_double_complex
call c_sub(z_double_complex, 24)
print*, "The actual value being:", z_double_complex
flush(output_unit)

!---- testing c_long_double_complex
call c_sub(z_long_double_complex, 25)
print*, "The actual value being:", z_long_double_complex
flush(output_unit)

!---- testing c_bool
call c_sub(l_bool, 26)
print*, "The actual value being:", l_bool
flush(output_unit)

!---- testing c_char
call c_sub(x_char, 27)
print*, "The actual value being:", x_char
flush(output_unit)

!---- testing c_size_t
call c_sub(i_size_t, 28)
print*, "The actual value being:", i_size_t
flush(output_unit)

end
