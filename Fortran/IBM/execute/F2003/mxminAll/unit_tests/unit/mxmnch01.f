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
      character(20) a, b, c, d, e, f, g, h, i, j, k
      character(20) u, v, w, x
      a = 'IBM Canada'
      b = 'IBM USA'
      c = 'abcdefghijk'
      d = 'abc'
      e = 'abcdefg'
      f = 'abcdefgh'
      g = 'abcdef'
      h = 'a'
      i = 'ab'
      j = 'abcde'
      k = 'ac'
      u = max(a, b)
      v = min(a, b)
      w = max(c, d, e, f, g, h, i, j, k)
      x = min(c, d, e, f, g, h, i, j, k)
      if (u < a) error stop 1
      if (v > b) error stop 2
      if (w /= 'ac') error stop 3
      if (x /= 'a') error stop 4
      end
