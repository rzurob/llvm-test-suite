!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AssumedTypeObj03f
!*
!*  PROGRAMMER                 : Ren, Jian Gang
!*  DATE                       : Apr 14, 2012
!*  ORIGIN                     : Linux/AIX Compiler Development,
!*                             : IBM Software Solutions China Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop Assumed-type object
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*
!*  DESCRIPTION                : Calling a non-BIND(C) procedure from Fortran
!*                               where the procedure is defined in C with
!*                               assumed-type object.
!*                               - Actual arg is an array 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program AssumedTypeObj03f
  use, intrinsic :: iso_c_binding
  implicit none

  integer(c_int)   :: i_int_arr(5)
  integer(C_int)   :: i_int_arr2(4)

  interface
    subroutine c_func_arr(a, len)  
      use, intrinsic :: iso_c_binding
      implicit none
      
      TYPE(*) :: a(*)
      integer(c_int), value :: len
    end subroutine c_func_arr
  end interface

  i_int_arr = [1, 2, 3, 4, 5]
  i_int_arr2 = [-1, -2, -3, -4]

  call c_func_arr(i_int_arr, 5)
  call c_func_arr(i_int_arr2, 4)
end
