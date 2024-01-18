!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfixed
! %GROUP: mxmnch09.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support character argument for MAX/
!*                               MIN/MAXVAL/MINVAL/MAXLOC/MINLOC
!*  SECONDARY FUNCTIONS TESTED : Functional test
!*
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
      character(3), parameter :: c = 'abc'
      character(3), parameter :: d = 'xyz'
      character(10) a
      character(2) b
      a = 'uvwxyz'
      b = 'mn'
      if (min(a, c) // b /= 'abc       mn') error stop 1
      if (max(a, d) // b /= 'xyz       mn') error stop 2
      if (len(max(a(1:3), c, d)) /= 3) error stop 3
      if (max(a(3:6), c, d) /= 'xyz') error stop 4
      if (len(min(a(1:3), c, d)) /= 3) error stop 5
      if (min(a(3:6), c, d) /= 'abc') error stop 6
      end
