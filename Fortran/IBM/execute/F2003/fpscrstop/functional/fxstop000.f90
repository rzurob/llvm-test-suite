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

program fxstop000

  use, intrinsic :: ieee_exceptions
  implicit none

  call ieee_set_flag(ieee_all, .true.)

  call s1()

  stop "End Main"

contains

  subroutine s1()
    call ieee_set_flag(ieee_divide_by_zero, .true.)
    stop "End s1"       ! only div-by-zero should be flagged.
  end subroutine

end program fxstop000
