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
!*  DESCRIPTION                : Diagnosis of dummy argument with both
!*                               OPTIONAL and VALUE attributes inside
!*                               an interface block
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use iso_c_binding
      implicit none

      interface
         subroutine foo(a,b,c) bind(c)
           import
           integer(c_int), optional, value :: a
           real(c_float) :: b
           value :: b
           optional :: b
           integer(c_int) :: c
           optional :: c
           value :: c
         end

         function bar(bars_dummy_arg) bind(c)
           import
           integer(c_int), optional :: bars_dummy_arg
           integer(c_int) :: bar
           value :: bars_dummy_arg
         end
      end interface

      end

