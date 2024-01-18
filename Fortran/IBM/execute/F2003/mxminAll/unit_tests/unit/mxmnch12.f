!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfixed
! %GROUP: mxmnch12.f 
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
!*  TEST CASE TITLE            : mxmnch12
!*
!*  PROGRAMMER                 : John Zang
!*  DATE                       : Oct. 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Support character argument for MAX/
!*                               MIN/MAXVAL/MINVAL/MAXLOC/MINLOC
!*  SECONDARY FUNCTIONS TESTED : Functional test
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : -qfixed
!*
!*  DESCRIPTION                : MAX/MIN - Maximum or minimum value
!*                               according to their collating sequence
!*                               of ASCII characters. 
!*                               MAXVAL/MINVAL - Maximum or minimum value
!*                               of elements in a character array.
!*                               MAXLOC/MINLOC - The location of maximum
!*                               or minimum value of elements in a character
!*                               array.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      integer i
      character(5), parameter :: a = 'a' 
      character(5), parameter :: b = 'b' 
      character(5), parameter :: c = 'c' 
      character(3) :: x(4) = (/(max(a, b, c), i = 1, 4)/)
      character(3) :: y(4) = (/(min(a, b, c), i = 1, 4)/)
      if (x(1) /= 'c') error stop 1
      if (x(2) /= 'c') error stop 2
      if (x(3) /= 'c') error stop 3
      if (x(4) /= 'c') error stop 4
      if (y(1) /= 'a') error stop 5
      if (y(2) /= 'a') error stop 6
      if (y(3) /= 'a') error stop 7
      if (y(4) /= 'a') error stop 8
      end
