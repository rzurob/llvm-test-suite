!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May 23, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop OPTIONAL argument
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Calling a BIND(C) procedure from Fortran
!*                               where the procedure is defined in C.
!*                               - The actual argument is not specified.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
         subroutine c_func(arg1, arg2) bind(C)
           import
           integer(c_int), optional :: arg1
           real(c_double), optional :: arg2
         end
      end interface

      integer(c_int) :: ii = -10
      real(c_double) :: rr = -3.14

      call c_func()
      call c_func(ii, rr)
      ii = -20
      call c_func(ii)
      rr = -rr
      call c_func(arg2=rr)


      end
