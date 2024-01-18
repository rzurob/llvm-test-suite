!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May 23, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostics for C-interop OPTIONAL argument
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : -qlanglvl checking for optional argument
!*                               of a bind(c) procedure.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use iso_c_binding
      implicit none

      interface
         subroutine foo(a) bind(c)
           import
           integer(c_int), optional :: a
         end subroutine

         function bar(bars_dummy_arg) bind(c)
           import
           integer(c_int), optional :: bars_dummy_arg
           integer(c_int) :: bar
         end function
      end interface

      end


      subroutine s1(arg1, arg2) bind(c)
        use iso_c_binding
        integer(c_int) :: arg
        integer(c_int), optional :: arg2
        if (present(arg2)) then
           arg1 = arg2
        else
           arg1 = -arg1
        end if
      end subroutine
