!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AssumedType15d
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
!*  DESCRIPTION                : Assumed type object as an actual argument 
!*                               that is argument associated with a dummy 
!*                               argument that is not assumed type
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
module  mod
  implicit none

  contains
  subroutine module_sub(c) 
     class(*) :: c

  end subroutine module_sub
end module mod

program AssumedType15d
use mod
implicit none
integer :: i 

i = 4
call sub(i)

contains 

   subroutine sub(a) 
      type(*) :: a

      call inner_sub(a)
      call module_sub(a)
   end subroutine sub

   subroutine inner_sub(b) 
      integer :: b

   end subroutine inner_sub

end program AssumedType15d
