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
      character(3), parameter :: a = 'abc'
      character(3), parameter :: b = 'uvw'
      character(3), parameter :: c = 'xyz'
      character(5) x(2), d
      d = 'ijk'
      x(1) = 'w'
      x(2) = 'z'
      if (max(x(1),x(2),a,b,c,d) /= 'z') error stop 1
      if (min(x(1),x(2),a,b,c,d) /= 'abc') error stop 2
      end