!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AssumedType22d
!*
!*  PROGRAMMER                 : Dorra Bouchiha
!*  DATE                       : June 13, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Assumed type dummy argument cannot be argument 
!*                               associated with an actual argument that is of 
!*                               derived type if it has a type-bound procedure
!* 
!*                              
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
module  mod
  implicit none

  type dt 
    integer :: cmp 

   contains
     procedure :: sum => my_sum
  end type dt

  contains
  subroutine module_sub(c) 
     type(*) :: c

  end subroutine module_sub

  integer function my_sum(this)
    class(dt), intent(in) :: this
    my_sum = 0
  end function my_sum

end module mod
program AssumedType22d
use mod
implicit none

type(dt) :: dt0

call sub(dt0)
call module_sub(dt0)

contains 

   subroutine sub(a) 
      type(*) :: a

   end subroutine sub
end program AssumedType22d
