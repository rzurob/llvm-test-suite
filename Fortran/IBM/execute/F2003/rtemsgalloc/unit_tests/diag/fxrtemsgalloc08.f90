!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxrtemsgalloc08.f
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
!*  TEST CASE TITLE            : fxrtemsgalloc08
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
!*  REQUIRED RUNTIME OPTIONS   : None. ( Default )
!*
!*  DESCRIPTION                : This diagnostic test, makes sure that
!*                               if allocate statement fails at runtime
!*                               line number and file name appear on
!*                               the error device before the message text.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      integer, allocatable :: a
      allocate(a)
      allocate(a)
      end
