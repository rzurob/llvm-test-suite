!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfixed
! %GROUP: mxmnch11.f 
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
!*  TEST CASE TITLE            : mxmnch11
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
      character(5) aa(2,2,2), bb(2,2,4), xx(2,2,2)
      aa = '!'
      bb = '!'
      aa(1,2,1) = 'abcd'
      aa(2,1,1) = 'uvwx'
      bb(1,2,1) = 'efgh'
      bb(2,1,1) = 'rstu'
      bb(2,1,3) = 'ijkl'
      bb(2,2,4) = 'mnop'
      xx = max(aa, bb(:,:,1:2), bb(:,:,3:4))
      if (xx(1,1,1) /= '!') error stop 1
      if (xx(2,1,1) /= 'uvwx') error stop 2
      if (xx(1,2,1) /= 'efgh') error stop 3
      if (xx(2,2,1) /= '!') error stop 4
      if (xx(1,1,2) /= '!') error stop 5
      if (xx(2,1,2) /= '!') error stop 6
      if (xx(1,2,2) /= '!') error stop 7
      if (xx(2,2,2) /= 'mnop') error stop 8
      aa = z'7f'
      bb = z'7f'
      aa(1,2,1) = 'abcd'
      aa(2,1,1) = 'uvwx'
      bb(1,2,1) = 'efgh'
      bb(2,1,1) = 'rstu'
      bb(2,1,3) = 'ijkl'
      bb(2,2,4) = 'mnop'
      xx = min(aa, bb(:,:,1:2), bb(:,:,3:4))
      if (xx(1,1,1) /= z'7f') error stop 9
      if (xx(1,2,1) /= 'abcd') error stop 10
      if (xx(2,1,1) /= 'ijkl') error stop 11
      if (xx(2,2,1) /= z'7f') error stop 12
      if (xx(1,1,2) /= z'7f') error stop 13
      if (xx(1,2,2) /= z'7f') error stop 14
      if (xx(2,1,2) /= z'7f') error stop 15
      if (xx(2,2,2) /= 'mnop') error stop 16
      end
