!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May 23, 2012
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop OPTIONAL argument
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Calling a BIND(C) procedure from Fortran
!*                               where the procedure is defined in Fortran.
!*                               - The actual argument is not specified.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: iso_c_binding
      implicit none

      interface
         subroutine foo(arg1, arg2) bind(C)
           import
           integer(c_int), optional :: arg1
           real(c_double), optional :: arg2
         end
      end interface

      integer(c_int) :: ii = -10
      real(c_double) :: rr = -3.14

      call foo()
      call foo(ii, rr)
      ii = -20
      call foo(ii)
      rr = -rr
      call foo(arg2=rr)


      end

      subroutine foo(a, b) bind(C)
        use, intrinsic :: iso_c_binding
        integer(c_int), optional :: a
        real(c_double), optional :: b

        if (present(a)) then
           print *, "a =", a
        else
           print *, "a = absent"
        end if
        if (present(b)) then
           print '(a5,F6.2)', "b = ", b
        else
           print *, "b = absent"
        end if
      end
