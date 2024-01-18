!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxrtemsgalloc02.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 31, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Providing file name and line number
!*                               when allocate or deallocate statements
!*                               fail at run-time. Customer requested
!*                               feature 305340.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*  REQUIRED RUNTIME OPTIONS   : XLFRTEOPTS=errloc=no
!*
!*  DESCRIPTION                : This diagnostic test, makes sure that
!*                               if deallocate statement fails at runtime
!*                               and errloc runtime option is set to "no",
!*                               line number and file name do not appear on
!*                               the error device before the message text.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer, allocatable :: a
      deallocate(a)
      end
