!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 20, 2005
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
      character(20), parameter :: c = 'abcdefghijk'
      character(5), parameter :: d = 'abc'
      character(4), parameter :: k = 'ac'
      character(50) e, f, g, h
      character(19) i
      character(51) j
      character(50) w, x
      e = 'abcdefg'
      f = 'abcdefgh'
      g = 'abcdef'
      h = 'a'
      i = 'ab'
      j = 'abcde'
      w = max(c, d, e, f, g, h, i, j, k)
      x = min(c, d, e, f, g, h, i, j, k)
      if (w /= 'ac') error stop 1
      if (x /= 'a') error stop 2
      if (len(max(c, d, e, f, g, h, i, j, k)) /= 51) error stop 3
      if (len(min(c, d, i)) /= 20) error stop 4
      end
