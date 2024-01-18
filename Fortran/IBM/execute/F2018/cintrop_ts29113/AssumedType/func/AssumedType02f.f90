!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 22, 2012
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Calling a BIND(C) procedure from Fortran
!*                               where the procedure is defined in C
!*                               Multiple TYPE(*) dummy arguments
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890

use, intrinsic :: iso_c_binding
implicit none
integer(c_int)                 :: i_int
real(c_float)                  :: r_float
character(c_char)              :: x_char


interface
   subroutine c_sub(a, b, c) BIND(c)
      use, intrinsic :: iso_c_binding
      implicit none
      type(*) :: a, b, c
   end subroutine c_sub
end interface

i_int = 4
r_float = 1.0
x_char = 'A'


print*, "Before the call to C_sub:", i_int, r_float, x_char
call c_sub(i_int, r_float, x_char)
print*, "After the call to C_sub:", i_int, r_float, x_char

end
