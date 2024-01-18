!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AssumedType06d
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
!*  DESCRIPTION                : TYPE(*) cannot appear in assignement statement
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
program AssumedType06d
implicit none
integer, parameter :: ten = 10
integer :: i 

i = 4
call sub(i)

contains 

   subroutine sub(a) 
      type(*) :: a

      a = ten 
   end subroutine sub

   subroutine another_sub(b) 
      type(*) :: b

      call inner_sub(b) 
   end subroutine another_sub

   subroutine inner_sub(c) 
      type(*) :: c

      c = i
   end subroutine inner_sub

end program AssumedType06d
