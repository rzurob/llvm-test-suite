!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 05, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : providing support for the DECIMAL=
!*                               specifier and decimal edit mode control
!*                               descriptors. Feature 289039.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*  REQUIRED RUNTIME OPTIONS   :
!*
!*  DESCRIPTION                : This diagnostic test, checks to make sure
!*                               when DECIMAL= specifier is used in
!*                               unformatted I/O, an error message is issued.
!*                               This tests the runtime messages.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      character(20) my_fmt

      my_fmt = "unformatted"

      open(unit=77, file='decimaleditd007.dat', form=my_fmt,    &
     &     decimal='comma')

      end
