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

! the main program calls procedure Q which uses neither ieee_arithmetic nor
! ieee_exceptions. Q changes the floating point status before returning. The
! example traces the changes to the floating point status before calling Q,
! on entry to Q, on exit from Q, and after returning from Q.

program main
  use ieee_arithmetic

  logical, dimension(5) :: flag_values
  type(ieee_round_type) :: round_value

  call ieee_set_flag(ieee_overflow, .true.)

  call ieee_get_flag(ieee_all, flag_values)
  print *, "main: flags ",flag_values

  call Q()

  call ieee_get_flag(ieee_all, flag_values)
  print *, "main: flags ",flag_values

  call ieee_get_rounding_mode(round_value)
  if (round_value == ieee_nearest) then
    print *, "main: rounding mode: ieee_nearest"
  endif
end program main

subroutine Q()
  use xlf_fp_util
  interface
    function get_flags()
      logical, dimension(5) :: get_flags
    end function
  end interface

  logical, dimension(5) :: flag_values
  integer(fp_mode_kind) :: oldmode

  flag_values = get_flags()
  print *, "   Q: flags on entry: ", flag_values

  call clr_fpscr_flags(fp_overflow)
  oldmode = set_round_mode(fp_rnd_rz)
  call set_fpscr_flags(trp_overflow)
  call set_fpscr_flags(fp_underflow)

  if (get_round_mode() == fp_rnd_rz) then
    print *, "   Q: rounding mode on exit: to_zero"
  endif

  flag_values = get_flags()
  print *, "   Q: flags on exit: ", flag_values
end subroutine Q

! print the status of all exception flags
function get_flags()
  use xlf_fp_util
  logical, dimension(5) :: get_flags
  integer(fpscr_kind), dimension(5) :: flags
  integer i

  flags = (/ fp_overflow, fp_div_by_zero, fp_invalid, &
  &          fp_underflow, fp_inexact /)
  do i=1,5
    get_flags(i) = (get_fpscr_flags(flags(i)) /= 0)
  end do
end function
