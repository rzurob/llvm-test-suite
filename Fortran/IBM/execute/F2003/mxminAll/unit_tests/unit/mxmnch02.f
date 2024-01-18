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
      character(20) a, b, c, d
      character(20) u, v, w, x
      a = 'IBM'
      b = ' Canada'
      c = ' USA'
      if (a // max(b, c) /= 'IBM                  USA') error stop 1
      if (a // min(b, c) /= 'IBM                  Canada') error stop 2
      end
