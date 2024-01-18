!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxrtemsgalloc12.f
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
!*  TEST CASE TITLE            : fxrtemsgalloc12
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Oct. 31, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Providing file name and line number
!*                               when allocate or deallocate statements
!*                               fail at run-time. Customer requested
!*                               feature 305340.
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : 
!*  REQUIRED RUNTIME OPTIONS   : None.
!*
!*  DESCRIPTION                : This diagnostic test, makes sure that
!*                               if an allocate statement that appears
!*                               in a file that is included in this file
!*                               fails at runtime, the correct filename
!*                               and line number is displayed.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      include "fxrtemsgalloc12a.f"
      end
