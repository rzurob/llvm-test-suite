!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AssumedType03d
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
!*  DESCRIPTION                : Calling a BIND(C) procedure from Fortran
!*                               where the procedure is defined in C
!*                               This test case focuses on C types that
!*                               are interoperable with INTEGER and REAL
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
module  mod
  implicit none

  contains
  subroutine module_sub(c) 
     type(*) :: c

  end subroutine module_sub
end module mod

program AssumedType03d
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
      type(*) :: b

   end subroutine inner_sub

end program AssumedType03d
