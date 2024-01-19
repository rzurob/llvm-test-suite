!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 09, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : providing support for the DECIMAL=
!*                               specifier and decimal edit mode control
!*                               descriptors. Feature 289039.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This diagnostic test, checks different
!*                               invalid forms of the dc and dp descriptors.
!*                               This test is for run-time encoding.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      character(50) :: my_fmt

      ! valid:
      my_fmt = "(dc,f4.2)"
      write(*,my_fmt) 3.14
      my_fmt = "(dp,f4.2)"
      write(*,my_fmt) 3.14

      ! invalid:
      my_fmt = "(3dc,f4.2)"
      write(*,my_fmt) 3.14
      my_fmt = "(3dp,f4.2)"
      write(*,my_fmt) 3.14
      my_fmt = "(dc4.2,f4.2)"
      write(*,my_fmt) 3.14
      my_fmt = "(dp4.2,f4.2)"
      write(*,my_fmt) 3.14
      my_fmt = "(dp.,f4.2)"
      write(*,my_fmt) 3.14
      my_fmt = "(.dc,f4.2)"
      write(*,my_fmt) 3.14

      end
