!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: decimaleditd015.f
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
!*  DESCRIPTION                : This diagnostic test, checks different
!*                               invalid forms of the dc and dp descriptors.
!*                               This test is for compile-time encoding.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      ! valid:
      write(*,"(dc,f4.2)") 3.14
      write(*,"(dp,f4.2)") 3.14

      ! invalid:
      write(*,"(3dc,f4.2)") 3.14
      write(*,"(3dp,f4.2)") 3.14
      write(*,"(dc4.2,f4.2)") 3.14
      write(*,"(dp4.2,f4.2)") 3.14
      write(*,"(dp.,f4.2)") 3.14
      write(*,"(.dc,f4.2)") 3.14

      end
