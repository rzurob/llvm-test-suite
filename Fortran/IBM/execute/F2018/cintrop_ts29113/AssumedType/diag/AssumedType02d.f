!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AssumedType01f
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
!*  DESCRIPTION                : Assumed type dummy argument accepted for 
!*                               procedures with and without BIND(C) 
!*                               with and without -qlanglvl=extended,
!*                               -qlanglvl=2008pure, -qlanglvl=2008std, 
!*                               -qlanglvl=ts
!*
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890

use, intrinsic :: iso_c_binding
implicit none
integer(c_int)         :: i_int   

interface
   subroutine f_sub(a) 
      implicit none
      type(*) :: a
   end subroutine f_sub
   subroutine c_sub(a) BIND(c)
      implicit none
      type(*) :: a
   end subroutine c_sub
end interface

call sub(i_int)
call f_sub(i_int)
call c_sub(i_int)

contains 
   subroutine sub(a) 
      type(*) :: a
   end subroutine sub
end 
