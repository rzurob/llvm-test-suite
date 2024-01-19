!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 16, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : When STOP stmt is executed and a floating-
!*                               point exception is occurred, an informational
!*                               message should be displayed indicating which
!*                               FPSCR exception flags are set.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : @PROCESS directive
!*  REQUIRED RUNTIME OPTIONS   :
!*
!*  DESCRIPTION                : Test the STOP message when all five
!*                               FPSCR exception flags are on.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
@PROCESS XLF2003(STOPEXCEPT)
      call s1()

      stop "End Main"

      end

@PROCESS XLF2003(NOSTOPEXCEPT)
      subroutine s1()
        use, intrinsic :: ieee_exceptions
        call ieee_set_flag(ieee_all, .true.)
        stop "End s1"
      end subroutine
