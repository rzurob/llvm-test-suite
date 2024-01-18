!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fpscrstop037.f
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
!*  TEST CASE TITLE            : fpscrstop037
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Feb. 21, 2006
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
!*  REQUIRED COMPILER OPTIONS  : -qxlf2003=stopexcept
!*  REQUIRED RUNTIME OPTIONS   : 
!*
!*  DESCRIPTION                : Test the STOP <digit-string> when it 
!*                               is not labeled and no exception flags
!*                               are signaling.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      
      use, intrinsic :: ieee_exceptions
      implicit none
      
      call ieee_set_flag(ieee_all, .false.)
      
      stop 1234
      
      end
