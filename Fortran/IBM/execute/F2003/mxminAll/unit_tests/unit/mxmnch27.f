!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfixed
! %GROUP: mxmnch27.f 
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
!*  TEST CASE TITLE            : mxmnch27
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
      character(1000) aa(2,2), x
      logical mm(2,2)
      integer xx(2)
      aa(1,1) = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaa'
      aa(2,1) = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaaa'
      aa(1,2) = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaaaa'
      aa(2,2) = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaaaaa'
      mm = .true.
      mm(2,2) = .false.
      x = maxval(aa, mask=mm)
      if (x(32:32) /= 'b') error stop 1
      xx = maxloc(aa, mask=mm)
      if (xx(1) /= 1) error stop 2
      if (xx(2) /= 2) error stop 3
      mm = .true.
      mm(1,1) = .false.
      x = minval(aa, mask=mm)
      if (x(33:33) /= 'b') error stop 4
      xx = minloc(aa, mask=mm)
      if (xx(1) /= 2) error stop 5
      if (xx(2) /= 1) error stop 6
      end
