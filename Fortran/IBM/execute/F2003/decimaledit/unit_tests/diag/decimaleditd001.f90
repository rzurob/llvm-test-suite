!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditd001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 25, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : providing support for the DECIMAL=
!*                               specifier and decimal edit mode control
!*                               descriptors. Feature 289039.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qdebug=intmsg
!*  REQUIRED RUNTIME OPTIONS   :
!*
!*  DESCRIPTION                : This diagnostic test, makes sure that
!*                               if the DECIMAL= specifier is used more
!*                               than once in the same i/o statement,
!*                               an appropriate error message is issued.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer :: ios
      character(5) mode
      character(10) :: buffer, tmp

      open(unit=1,file='decimaleditd001.dat',decimal='comma',          &
     & decimal='comma')
      write(*,*, decimal='comma', iostat=ios, decimal='comma') 3.14
      read(1, *, decimal='point', iostat=ios, decimal='comma') tmp
      inquire(unit=1, decimal=mode, iostat=ios, decimal=mode)

      ! test for internal files as well:
      write(buffer,*, decimal='comma', iostat=ios, decimal='comma') 3.14
      read(buffer, *, decimal='point', iostat=ios, decimal='comma') tmp

      end
