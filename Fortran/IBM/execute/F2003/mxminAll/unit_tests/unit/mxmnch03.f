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
      character(2) a
      character(3) b
      character(8) c
      character(15) d
      a = 'bc'
      b = 'abx'
      c = 'abcde'
      d = 'abcdefghijklmno'
      if (len(max(a, b, c, d)) /= 15 .and. max(a, b, c, d) /= 'bc')
     + error stop 1
      if (len(min(a, b, c, d)) /= 15 .and. max(a, b, c, d) /= 'abcde')
     + error stop 2
      end