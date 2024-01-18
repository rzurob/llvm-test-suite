!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr 14, 2012
!*  ORIGIN                     : Linux/AIX Compiler Development,
!*                             : IBM Software Solutions China Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop Assumed-type object
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Calling a non-BIND(C) procedure from Fortran
!*                               where the procedure is defined in C with
!*                               assumed-type object.
!*                               - Actual arg is of type intrinsic
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program AssumedTypeObj02f
  use, intrinsic :: iso_c_binding
  implicit none

  integer(c_short) :: i_short
  integer(c_int)   :: i_int
  integer(c_long)  :: i_long
  integer(c_long_long) :: i_long_long

  interface
    subroutine c_func(a, flag)
      use, intrinsic :: iso_c_binding
      implicit none

      TYPE(*) :: a
      integer(c_int), value :: flag
    end subroutine c_func
  end interface

  i_short = 2
  i_int = 4
  i_long = 4
  i_long_long = 8

  call c_func(i_short, 1)
  call c_func(i_int, 2)
  call c_func(i_long, 3)
  call c_func(i_long_long, 4)

end
