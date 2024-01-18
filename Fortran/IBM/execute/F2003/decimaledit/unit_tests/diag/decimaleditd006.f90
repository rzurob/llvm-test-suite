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
!*  REQUIRED COMPILER OPTIONS  : -qdebug=intmsg
!*  REQUIRED RUNTIME OPTIONS   :
!*
!*  DESCRIPTION                : This diagnostic test, checks to make sure
!*                               when DECIMAL= specifier is used in
!*                               unformatted I/O, an error message is issued.
!*                               This tests the compile-time messages.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      implicit none

      character(20) buffer

      open(unit=77, file='decimaleditd006.dat', form='unformatted',    &
     &     decimal='comma')

      write(77,decimal='comma') 3.14

      ! try the same trick for internal files. ( compiler should catch it )
      write(buffer,decimal='comma') 3.14

      end
