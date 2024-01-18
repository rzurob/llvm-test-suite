!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 13, 2012
!*  ORIGIN                     : AIX Compiler Development,
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
implicit none
integer, parameter             :: N = 10
integer                        :: i
integer(c_short)               :: i_short(N)
integer(c_int)                 :: i_int(N)
integer(c_long)                :: i_long(N/2)
real(c_float)                  :: r_float(2*N)
real(c_double)                 :: r_double(N/5)
complex(c_float_complex)       :: z_float_complex(N/N)
logical(c_bool)                :: l_bool(3)


interface
   subroutine c_sub(a, buffer_size, type_flag) BIND(c)
      use, intrinsic :: iso_c_binding
      implicit none
      type(*) :: a
      integer(c_int), value :: buffer_size, type_flag
   end subroutine c_sub
end interface

i_int = [(i*2, i=1,N)]
i_long = 4
i_short = [(i, i=1,N)]
r_float = 1.03
r_double = 9.25
z_float_complex = (5.0e0,5.0e0)        !<--- does not compile
l_bool = .true.

print*, "Before the call to C_sub:", i_short
call c_sub(i_short(1), size(i_short), 1)                     !<--- modify all the elements
print*, "After the call to C_sub:", i_short

print*, "Before the call to C_sub:", i_int
call c_sub(i_int(5), (N-5)+1, 2)                             !<--- modify 6 elements: 5 to 10
print*, "After the call to C_sub:", i_int

print*, "Before the call to C_sub:", i_long
call c_sub(i_long(size(i_long)), 1, 3)                       !<--- modify only the passed element (the last one)
print*, "After the call to C_sub:", i_long

print*, "Before the call to C_sub:", r_float
call c_sub(r_float(1), 1, 4)                                 !<--- modify only the passed element (the first one)
print*, "After the call to C_sub:", r_float

print*, "Before the call to C_sub:", r_double
call c_sub(r_double(1), 2, 5)                                !<--- modify the whole array (2 elements)
print*, "After the call to C_sub:", r_double

print*, "Before the call to C_sub:", z_float_complex
call c_sub(z_float_complex(1), size(z_float_complex), 6)    !<------  modify the whole array (1 element)
print*, "After the call to C_sub:", z_float_complex

print*, "Before the call to C_sub:", l_bool
call c_sub(l_bool(1), size(l_bool), 7)                       !<--- modify the whole array (3 elements)
print*, "After the call to C_sub:", l_bool

end
