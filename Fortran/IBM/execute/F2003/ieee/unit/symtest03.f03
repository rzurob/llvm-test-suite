! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March, 2002
!*
!*  PRIMARY FUNCTIONS TESTED   : Save and Restore
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : Test save and restore
!*                               These tests appear in the docs
!*
!234567890123456789012345678901234567890123456789012345678901234567890

! The main program calls procedure P which uses the ieee_arithmetic module.
! P changes the floating point status before returning. The example traces
! the changes to the floating point status before calling P, on entry to P,
! on exit from P, and after returning from P.

program main
  use ieee_arithmetic

  interface
    subroutine P()
      use ieee_arithmetic
    end subroutine P
  end interface

  logical, dimension(5) :: flag_values
  type(ieee_round_type) :: round_value

  call ieee_set_flag(ieee_overflow, .true.)

  call ieee_get_flag(ieee_all, flag_values)
  print *, "main: flags ",flag_values

  call P()

  call ieee_get_flag(ieee_all, flag_values)
  print *, "main: flags ",flag_values

  call ieee_get_rounding_mode(round_value)
  if (round_value == ieee_nearest) then
    print *, "main: rounding mode: ieee_nearest"
  endif
end program main


subroutine P()
  use ieee_arithmetic
  logical, dimension(5) :: flag_values
  type(ieee_round_type) :: round_value

  call ieee_get_flag(ieee_all, flag_values)
  print *, "   P: flags on entry: ",flag_values

  call ieee_set_rounding_mode(ieee_to_zero)
  call ieee_set_halting_mode(ieee_overflow, .true.)
  call ieee_set_flag(ieee_underflow, .true.)

  call ieee_get_rounding_mode(round_value)
  if (round_value == ieee_to_zero) then
    print *, "   P: rounding mode on exit: ieee_to_zero"
  endif
  call ieee_get_flag(ieee_all, flag_values)
  print *, "   P: flags on exit: ",flag_values
end subroutine P
