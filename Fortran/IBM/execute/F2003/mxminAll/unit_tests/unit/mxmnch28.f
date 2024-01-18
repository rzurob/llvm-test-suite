!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfixed
! %GROUP: mxmnch28.f
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
      character(1000) aa(2,2), x
      logical mm(2,2)
      integer xx(2), l, h, s
      l = 1
      h = 2
      s = 1
      aa(1,1) = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaa'
      aa(2,1) = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaaa'
      aa(1,2) = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaaaa'
      aa(2,2) = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaaaaa'
      mm = .true.
      mm(2,2) = .false.
      x = maxval(aa(l:h:s,l:h:s), mask=mm)
      if (x(32:32) /= 'b') error stop 1
      xx = maxloc(aa(l:h:s,l:h:s), mask=mm)
      if (xx(1) /= 1) error stop 2
      if (xx(2) /= 2) error stop 3
      mm = .true.
      mm(1,1) = .false.
      x = minval(aa(l:h:s,l:h:s), mask=mm)
      if (x(33:33) /= 'b') error stop 4
      xx = minloc(aa(l:h:s,l:h:s), mask=mm)
      if (xx(1) /= 2) error stop 5
      if (xx(2) /= 1) error stop 6
      end
