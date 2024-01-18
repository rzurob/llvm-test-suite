!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfixed
! %GROUP: mxmnch21.f
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
      character(10), parameter :: aa(2,2) =
     + reshape((/'a','b','c','d'/), (/2,2/))
      character(10) bb(2,3,4)
      character(10) xx(3,4)
      bb = '!'
      bb(2,1,3) = 'ibm'
      bb(1,2,4) = 'usa'
      bb(1,2,2) = 'can'
      bb(1,3,1) = 'bel'
      bb(2,2,1) = 'jan'
      bb(2,3,4) = 'gb'

      xx = maxval(bb, dim=1)
      if (maxval(aa) /= 'd') error stop 1
      if (xx(1,1) /= '!') error stop 2
      if (xx(2,1) /= 'jan') error stop 3
      if (xx(3,1) /= 'bel') error stop 4
      if (xx(1,2) /= '!') error stop 5
      if (xx(2,2) /= 'can') error stop 6
      if (xx(3,2) /= '!') error stop 7
      if (xx(1,3) /= 'ibm') error stop 8
      if (xx(2,3) /= '!') error stop 9
      if (xx(3,3) /= '!') error stop 10
      if (xx(1,4) /= '!') error stop 11
      if (xx(2,4) /= 'usa') error stop 12
      if (xx(3,4) /= 'gb') error stop 13
      bb = z'7f'
      bb(2,1,3) = 'ibm'
      bb(1,2,4) = 'usa'
      bb(1,2,2) = 'can'
      bb(1,3,1) = 'bel'
      bb(2,2,1) = 'jan'
      bb(2,3,4) = 'gb'
      xx = minval(bb, dim=1)
      if (minval(aa) /= 'a') error stop 14
      if (xx(1,1) /= z'7f') error stop 15
      if (xx(2,1) /= 'jan') error stop 16
      if (xx(3,1) /= 'bel') error stop 17
      if (xx(1,2) /= z'7f') error stop 18
      if (xx(2,2) /= 'can') error stop 19
      if (xx(3,2) /= z'7f') error stop 20
      if (xx(1,3) /= 'ibm') error stop 21
      if (xx(2,3) /= z'7f') error stop 22
      if (xx(3,3) /= z'7f') error stop 23
      if (xx(1,4) /= z'7f') error stop 24
      if (xx(2,4) /= 'usa') error stop 25
      if (xx(3,4) /= 'gb') error stop 26
      end
