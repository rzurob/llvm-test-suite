!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : November 13, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Rank objects
!*  SECONDARY FUNCTIONS TESTED : Assumed Type
!*
!* REQUIRED COMPILER OPTIONS    :
!*                               (use -D_DEBUG for a debug version)
!*
!* DESCRIPTION                  : Calling a BIND(C) procedure defined in C from Fortran
!*                                - explicit shape arrays of various shapes
!*                                - CFI_attribute_other
!*                                - all interoperable intrinsic data types
!*
!* Actua15 Argument:
!*
!* Dummy Argument:
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
   use, intrinsic :: iso_c_binding

   integer, parameter :: N = 22

   type, bind(c) :: base
      character(c_char) :: a = "A"
      integer(c_int)    :: b = 1
   end type

   type, bind(c) :: ext
      character(c_char) :: a = "B"
      integer(c_int) :: b = 2
      type(base) :: d0
   end type

end module mod

program AssumedRank313f
use mod
use iso_fortran_env
implicit none
integer(c_short)               :: i_short(2,2)
integer(c_int)                 :: i_int(8)
integer(c_long)                :: i_long(4)
integer(c_long_long)           :: i_long_long(2)
integer(c_signed_char)         :: i_signed_char(1,2,3,4)
integer(c_int8_t)              :: i_int8_t(-10:10,10:20)
integer(c_int16_t)             :: i_int16_t(3:4,0:0)
integer(c_int32_t)             :: i_int32_t(1)
integer(c_int64_t)             :: i_int64_t(100)
integer(c_int_least8_t)        :: i_int_least8_t(1)
integer(c_int_least16_t)       :: i_int_least16_t(1,1)
integer(c_int_least32_t)       :: i_int_least32_t(1,1,1)
integer(c_int_least64_t)       :: i_int_least64_t(1,1,1,1)
integer(c_int_fast8_t)         :: i_int_fast8_t(2,2,2,2,2)
integer(c_int_fast16_t)        :: i_int_fast16_t(2,2,2,2,2,2)
integer(c_int_fast32_t)        :: i_int_fast32_t(2,2,2,2,2,2,2)
integer(c_int_fast64_t)        :: i_int_fast64_t(2,2,2,2,2,2,2,2)
integer(c_intmax_t)            :: i_intmax_t(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
integer(c_intptr_t)            :: i_intptr_t(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
real(c_float)                  :: r_float(-2:2,1,333:335)
real(c_double)                 :: r_double(N)
real(c_long_double)            :: r_long_double(100)
complex(c_float_complex)       :: z_float_complex(5,5)
complex(c_double_complex)      :: z_double_complex(-5:-1,-4:0)
complex(c_long_double_complex) :: z_long_double_complex(1)
logical(c_bool)                :: l_bool(1,1,1,1,1,1,1,1,1,1,1,1,1,10)
character(c_char)              :: x_char(1024)
type(base)                     :: dt0(1,2,3,4,5)
type(ext)                      :: dt1(N/2)


interface
   subroutine c_sub(a, flag) BIND(c)
      use, intrinsic :: iso_c_binding
      implicit none
      type(*) :: a(..)
      integer(c_int), value :: flag
   end subroutine c_sub
end interface

i_int = 4
i_long = -32767
i_short = 2
i_long_long = 8
i_signed_char = -127
i_int8_t = 23567
i_int16_t = 47077
i_int32_t = -8888
i_int64_t = 160001
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
print*, "The actual value being:", i_short(1,1)
flush(output_unit)

!---- testing int
call c_sub(i_int, 2)
print*, "The actual value being:", i_int(1)
flush(output_unit)

!---- testing long
call c_sub(i_long, 3)
print*, "The actual value being:", i_long(1)
flush(output_unit)

!---- testing long long
call c_sub(i_long_long, 4)
print*, "The actual value being:", i_long_long(1)
flush(output_unit)

!---- testing c_signed_char
call c_sub(i_signed_char, 5)
print*, "The actual value being:", i_signed_char(1,1,1,1)
flush(output_unit)

!---- testing c_int8_t
call c_sub(i_int8_t, 6)
print*, "The actual value being:", i_int8_t(-10,10)
flush(output_unit)

!---- testing c_int16_t
call c_sub(i_int16_t, 7)
print*, "The actual value being:", i_int16_t(3,0)
flush(output_unit)

!---- testing c_int32_t
call c_sub(i_int32_t, 8)
print*, "The actual value being:", i_int32_t(1)
flush(output_unit)

!---- testing c_int64_t
call c_sub(i_int64_t, 9)
print*, "The actual value being:", i_int64_t(1)
flush(output_unit)

!---- testing c_int_least8_t
call c_sub(i_int_least8_t, 10)
print*, "The actual value being:", i_int_least8_t(1)
flush(output_unit)

!---- testing c_int_least16_t
call c_sub(i_int_least16_t, 11)
print*, "The actual value being:", i_int_least16_t(1,1)
flush(output_unit)

!---- testing c_int_least32_t
call c_sub(i_int_least32_t, 12)
print*, "The actual value being:", i_int_least32_t(1,1,1)
flush(output_unit)

!---- testing c_int_least64_t
call c_sub(i_int_least64_t, 13)
print*, "The actual value being:", i_int_least64_t(1,1,1,1)
flush(output_unit)

!---- testing c_int_fast8_t
call c_sub(i_int_fast8_t, 14)
print*, "The actual value being:", i_int_fast8_t(1,1,1,1,1)
flush(output_unit)

!---- testing c_int_fast16_t
call c_sub(i_int_fast16_t, 15)
print*, "The actual value being:", i_int_fast16_t(1,1,1,1,1,1)
flush(output_unit)

!---- testing c_int_fast32_t
call c_sub(i_int_fast32_t, 16)
print*, "The actual value being:", i_int_fast32_t(1,1,1,1,1,1,1)
flush(output_unit)

!---- testing c_int_fast64_t
call c_sub(i_int_fast64_t, 17)
print*, "The actual value being:", i_int_fast64_t(1,1,1,1,1,1,1,1)
flush(output_unit)

!---- testing c_intmax_t
call c_sub(i_intmax_t, 18)
print*, "The actual value being:", i_intmax_t(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
flush(output_unit)

!---- testing c_intptr_t
call c_sub(i_intptr_t, 19)
print*, "The actual value being:", i_intptr_t(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
flush(output_unit)

!---- testing c_float
call c_sub(r_float, 20)
print*, "The actual value being:", r_float(-2,1,333)
flush(output_unit)

!---- testing c_double
call c_sub(r_double, 21)
print*, "The actual value being:", r_double(1)
flush(output_unit)

!---- testing c_long_double
call c_sub(r_long_double, 22)
print*, "The actual value being:", r_long_double(1)
flush(output_unit)

!---- testing c_float_complex
call c_sub(z_float_complex, 23)
print*, "The actual value being:", z_float_complex(1,1)
flush(output_unit)

!---- testing c_double_complex
call c_sub(z_double_complex, 24)
print*, "The actual value being:", z_double_complex(-5,-4)
flush(output_unit)

!---- testing c_long_double_complex
call c_sub(z_long_double_complex, 25)
print*, "The actual value being:", z_long_double_complex(1)
flush(output_unit)

!---- testing c_bool
call c_sub(l_bool, 26)
print*, "The actual value being:", l_bool(1,1,1,1,1,1,1,1,1,1,1,1,1,1)
flush(output_unit)

!---- testing c_char
call c_sub(x_char, 27)
print*, "The actual value being: ", x_char(1)
flush(output_unit)

!---- testing derived type
call c_sub(dt0, 28)
print*, "The actual value being:", dt0(1,1,1,1,1)
flush(output_unit)

call c_sub(dt1, 29)
print*, "The actual value being:", dt1(1)
flush(output_unit)

end program AssumedRank313f
