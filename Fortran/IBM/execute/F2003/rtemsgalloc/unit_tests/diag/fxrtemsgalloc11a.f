!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxrtemsgalloc11.f
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
!*  REQUIRED RUNTIME OPTIONS   : None.
!*
!*  DESCRIPTION                : This is the file to be included in
!*                               fxrtemsgalloc11.f
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      integer, allocatable :: x(:)
      deallocate(x)


