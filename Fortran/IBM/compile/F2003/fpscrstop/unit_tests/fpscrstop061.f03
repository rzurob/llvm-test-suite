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
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=stopexcept and -qlanglvl and @PROCESS
!*
!*  DESCRIPTION                : diagnostics for langlvl
!*
!234567890123456789012345678901234567890123456789012345678901234567890

@PROCESS XLF2003(NOSTOPEXCEPT)
      REAL RL1
      REAL RL2
      REAL RL3

      RL1 = 3.0
      RL2 = 1.0
      RL3 = RL2 / RL1
      RL2 = RL3

      STOP

      END
