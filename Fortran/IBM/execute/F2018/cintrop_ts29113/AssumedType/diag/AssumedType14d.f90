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
!*  DESCRIPTION                : Assumed type objects cannot appear as an
!*                               argument of an intrinsic procedure other
!*                               than the ones listed above (Addition, Multiplication, MAX, MIN, ...)
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
module  mod
  implicit none

  contains
  integer function module_fnc(a,b,c)
     type(*) :: a, b, c

     module_fnc = max(a,b,c)

  end function module_fnc
end module mod

program AssumedType14d
use, intrinsic :: iso_c_binding
implicit none

contains

   type(*) function fnc0()
   end function fnc0

   integer function fnc(a,b)
      type(*), intent(inout) :: a
      type(*) :: b

      fnc = a*b
   end function fnc

end program AssumedType14d
