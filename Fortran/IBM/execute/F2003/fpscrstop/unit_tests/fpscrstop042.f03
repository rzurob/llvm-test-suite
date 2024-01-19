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
!*  DESCRIPTION                : Test the STOP <char-const> when some
!*                               exceptions are signaling. STOP is labeled.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      use, intrinsic :: ieee_exceptions
      implicit none

      call ieee_set_flag(ieee_underflow, .true.)
      call ieee_set_flag(ieee_invalid, .true.)

      goto 10

 10   stop "\"The End\""

      end
