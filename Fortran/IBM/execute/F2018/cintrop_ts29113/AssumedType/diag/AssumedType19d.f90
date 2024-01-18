!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AssumedType19d
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
!*  DESCRIPTION                : An assumed-type entity shall be a dummy variable 
!*                               that does not have the VALUE attribute
!*       
!*      
!*     
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
module  mod
  implicit none

  contains
  subroutine module_sub(c) 
     type(*) :: c(100)

  end subroutine module_sub
end module mod

program AssumedType19d
implicit none
integer, parameter :: N = 100

interface
   subroutine f_sub(a)
      implicit none
      type(*) :: a(4)
   end subroutine f_sub
   subroutine c_sub(a) BIND(c)
      implicit none
      type(*) :: a(10)
   end subroutine c_sub
end interface

contains 
   subroutine sub(a) 
      type(*) :: a(N)

   end subroutine sub
end program AssumedType19d
