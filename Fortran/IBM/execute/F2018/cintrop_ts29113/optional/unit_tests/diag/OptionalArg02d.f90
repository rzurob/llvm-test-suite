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
!*                               a procedure definition.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      subroutine foo(argument_one,b,c) bind(c)
        use iso_c_binding
        implicit none
        integer(c_int), optional, value :: argument_one
        real(c_float) :: b
        value :: b
        optional :: b
        integer(c_int) :: c
        optional :: c
        value :: c
      end

      function bar(d) bind(c)
        use iso_c_binding
        implicit none
        integer(c_int), optional :: d
        integer(c_int) :: bar
        value :: d
        bar = 0
      end
