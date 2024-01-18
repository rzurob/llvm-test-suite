!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfixed
! %GROUP: mxmnch20.f 
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
!*  TEST CASE TITLE            : mxmnch20
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
      character(10), parameter :: aa(2,2) = 
     + reshape((/'a','b','c','d'/), (/2,2/))
      character(10) bb(2,3,4)
      character(10) xx(3,4), yy(2,4), zz(2,3)

      bb = '!'
      bb(2,1,3) = 'ibm'
      bb(1,2,4) = 'usa'
      bb(1,2,2) = 'can'
      bb(1,3,1) = 'bel'
      bb(2,2,1) = 'jan'
      bb(2,3,4) = 'gb'
      
      xx = maxval(bb, dim=1,mask=bb .ne. 'x')
      if (maxval(aa) /= 'd') error stop 1
      if (maxval(bb) /= 'usa') error stop 2
      if (xx(1,1) /= '!') error stop 3
      if (xx(2,1) /= 'jan') error stop 4
      if (xx(3,1) /= 'bel') error stop 5
      if (xx(1,2) /= '!') error stop 6
      if (xx(2,2) /= 'can') error stop 7
      if (xx(3,2) /= '!') error stop 8
      if (xx(1,3) /= 'ibm') error stop 9
      if (xx(2,3) /= '!') error stop 10
      if (xx(3,3) /= '!') error stop 11
      if (xx(1,4) /= '!') error stop 12
      if (xx(2,4) /= 'usa') error stop 13
      if (xx(3,4) /= 'gb') error stop 14

      yy = maxval(bb, dim=2,mask=bb .ne. 'a')
      if (yy(1,1) /= 'bel') error stop 15
      if (yy(2,1) /= 'jan') error stop 16
      if (yy(1,2) /= 'can') error stop 17
      if (yy(2,2) /= '!') error stop 18
      if (yy(1,3) /= '!') error stop 19
      if (yy(2,3) /= 'ibm') error stop 20
      if (yy(1,4) /= 'usa') error stop 21
      if (yy(2,4) /= 'gb') error stop 22

      zz = maxval(bb,dim=3)
      if (zz(1,1) /= '!') error stop 23
      if (zz(2,1) /= 'ibm') error stop 24
      if (zz(1,2) /= 'usa') error stop 25
      if (zz(2,2) /= 'jan') error stop 26
      if (zz(1,3) /= 'bel') error stop 27
      if (zz(2,3) /= 'gb') error stop 28

      bb = z'7f'
      bb(2,1,3) = 'ibm'
      bb(1,2,4) = 'usa'
      bb(1,2,2) = 'can'
      bb(1,3,1) = 'bel'
      bb(2,2,1) = 'jan'
      bb(2,3,4) = 'gb'

      xx = minval(bb, dim=1,mask=bb .ne. 'abcd')
      if (minval(aa) /= 'a') error stop 29
      if (minval(bb) /= 'bel') error stop 30
      if (xx(1,1) /= z'7f') error stop 31
      if (xx(2,1) /= 'jan') error stop 32
      if (xx(3,1) /= 'bel') error stop 33
      if (xx(1,2) /= z'7f') error stop 34
      if (xx(2,2) /= 'can') error stop 35
      if (xx(3,2) /= z'7f') error stop 36
      if (xx(1,3) /= 'ibm') error stop 37
      if (xx(2,3) /= z'7f') error stop 38
      if (xx(3,3) /= z'7f') error stop 39
      if (xx(1,4) /= z'7f') error stop 40
      if (xx(2,4) /= 'usa') error stop 41
      if (xx(3,4) /= 'gb') error stop 42

      yy = minval(bb, dim=2)
      if (yy(1,1) /= 'bel') error stop 43
      if (yy(2,1) /= 'jan') error stop 44
      if (yy(1,2) /= 'can') error stop 45
      if (yy(2,2) /= z'7f') error stop 46
      if (yy(1,3) /= z'7f') error stop 47
      if (yy(2,3) /= 'ibm') error stop 48
      if (yy(1,4) /= 'usa') error stop 49
      if (yy(2,4) /= 'gb') error stop 50

      zz = minval(bb,dim=3)
      if (zz(1,1) /= z'7f') error stop 51
      if (zz(2,1) /= 'ibm') error stop 52
      if (zz(1,2) /= 'can') error stop 53
      if (zz(2,2) /= 'jan') error stop 54
      if (zz(1,3) /= 'bel') error stop 55
      if (zz(2,3) /= 'gb') error stop 56
      end
