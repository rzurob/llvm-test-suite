!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditd014.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 09, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : providing support for the DECIMAL=
!*                               specifier and decimal edit mode control
!*                               descriptors. Feature 289039.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qdebug=intmsg
!*
!*  DESCRIPTION                : This diagnostic test, checks to make sure
!*                               that decimal= specifier is only allowed in
!*                               inquire, read, write and open stmts.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      real :: input

      ! these three are valid:
      open(unit=77, file='decimaleditd014.dat', decimal='comma')
      read(77, *, decimal='point') input
      write(77,*,decimal='comma') 3.14

      ! these are not valid:
      backspace(77,decimal='point')
      rewind(77,decimal='point')
      endfile(77, decimal='comma')
      flush(77,decimal='comma')
      wait(id=77,decimal='point')
      close(77, decimal='comma')

      end
