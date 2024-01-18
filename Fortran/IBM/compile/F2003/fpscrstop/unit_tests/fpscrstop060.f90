!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fpscrstop060.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : fpscrstop060
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Feb. 16, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : When STOP stmt is executed and a floating-
!*                               point exception is occurred, an informational
!*                               message should be displayed indicating which
!*                               FPSCR exception flags are set.
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=nostopexcept and -qlanglvl and @PROCESS
!*
!*  DESCRIPTION                : diagnostics for langlvl
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      
@PROCESS XLF2003(STOPEXCEPT)
      REAL RL1
      REAL RL2
      REAL RL3
      
      RL1 = 3.0
      RL2 = 1.0
      RL3 = RL2 / RL1
      RL2 = RL3
      
      STOP
      
      END
