!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Functional test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : The procedure name inside an abstract
!*                               interface is treated as a local identifier.
!*                               It can be the same as a common block name.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
abstract interface
   integer function fun(a)
     real a
   end function
end interface
integer var, arg, num
common /fun/ var
var = 10
num = square(10)
if ((var .ne. 100) .or. (num .ne. 10000)) then
   error stop 1
endif
contains
function square(a)
   integer a, b, square
   common /fun/ b
   b = a * a
   square = var * var
end function
end
