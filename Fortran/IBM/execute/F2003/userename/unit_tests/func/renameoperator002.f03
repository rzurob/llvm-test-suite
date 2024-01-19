!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 28, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Rename operator in  USE statement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : functional testing of renaming the operator in
!*                               a USE statement
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module temp

  type real_num
    real :: x

   contains
     procedure :: real_add

     generic :: operator(.add.) => real_add

  end type

  interface operator (.add.)
    procedure int_add
  end interface

  contains
    function real_add(a,b)
      type(real_num) :: real_add
      class(real_num), intent(in) :: a,b
      real_add%x = a%x+b%x
    end function real_add

    function int_add(c,d)
      integer :: int_add
      integer, intent(in) ::c,d
      int_add=c+d
   end function int_add

end module


program main
use temp , operator(.plus.) => operator(.add.)
  integer :: a,b,c
  c=a.plus.b
end program