!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 21, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : When STOP stmt is executed and a floating-
!*                               point exception is occurred, an informational
!*                               message should be displayed indicating which
!*                               FPSCR exception flags are set.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=stopexcept
!*  REQUIRED RUNTIME OPTIONS   :
!*
!*  DESCRIPTION                : Miscellaneous: if exception occurs in the callee
!*                               and the stop in the caller is reached, those
!*                               exceptions in the callee must have been cleared.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxstop001

  use, intrinsic :: ieee_exceptions
  implicit none

  integer res

  call s1()

  res = f1()

  stop "End Main"

contains

  subroutine s1()
    call ieee_set_flag(ieee_divide_by_zero, .true.)
  end subroutine

  integer function f1()
    call ieee_set_flag(ieee_underflow, .true.)
    call ieee_set_flag(ieee_inexact, .true.)
    stop "End f1"        ! div-by-zero should not be flagged.
  end function

end program fxstop001