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
!*                               - Actual arg is of derived-type.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program AssumedTypeObj05f
  use, intrinsic :: iso_c_binding
  implicit none

  type c_intr1
    integer(c_int) :: a = 5
    integer(c_short) :: b = 10
    real(c_double) :: c = 1.2
  end type

  type(c_intr1) :: c_op

  call sub(c_op)

  c_op%a = 8
  c_op%b = 7
  c_op%c = 2.7

  call sub(c_op)

  contains
    subroutine sub(c_intr1)
      TYPE(*) :: c_intr1
      integer :: i

      interface
        integer function c_func_print_str(c_intr1)
          TYPE(*) :: c_intr1
        end function
      end interface

      i = c_func_print_str(c_intr1)

      if (i <> 15) then
        ERROR STOP 1
      end if
    end
end
