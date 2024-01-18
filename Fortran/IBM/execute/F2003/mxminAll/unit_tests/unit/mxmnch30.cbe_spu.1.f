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
      character(10), parameter :: aa(2,2) =
     + reshape((/'a','b','c','d'/), (/2,2/))
      character(10) bb(2,3,4)
      integer uu(2), ww(2,3), xx(3), yy(3,4), zz(2,4)

      bb = '!'
      bb(2,1,3) = 'ibm'
      bb(1,2,4) = 'usa'
      bb(1,2,2) = 'can'
      bb(1,3,1) = 'bel'
      bb(2,2,1) = 'jan'
      bb(2,3,4) = 'gb'

      ww = maxloc(bb,dim=3)
      if (ww(1,1) /= 1) error stop 1
      if (ww(2,1) /= 3) error stop 2
      if (ww(1,2) /= 4) error stop 3
      if (ww(2,2) /= 1) error stop 4
      if (ww(1,3) /= 1) error stop 5
      if (ww(2,3) /= 4) error stop 6

      xx = maxloc(bb)
      if (xx(1) /= 1) error stop 7
      if (xx(2) /= 2) error stop 8
      if (xx(3) /= 4) error stop 9

      yy = maxloc(bb, dim=1,mask=bb .ne. 'x')
      if (yy(1,1) /= 1) error stop 10
      if (yy(2,1) /= 2) error stop 11
      if (yy(3,1) /= 1) error stop 12
      if (yy(1,2) /= 1) error stop 13
      if (yy(2,2) /= 1) error stop 14
      if (yy(3,2) /= 1) error stop 15
      if (yy(1,3) /= 2) error stop 16
      if (yy(2,3) /= 1) error stop 16
      if (yy(3,3) /= 1) error stop 17
      if (yy(1,4) /= 1) error stop 18
      if (yy(2,4) /= 1) error stop 19
      if (yy(3,4) /= 2) error stop 20

      zz = maxloc(bb, dim=2,mask=bb .ne. 'a')
      if (zz(1,1) /= 3) error stop 21
      if (zz(2,1) /= 2) error stop 22
      if (zz(1,2) /= 2) error stop 23
      if (zz(2,2) /= 1) error stop 24
      if (zz(1,3) /= 1) error stop 25
      if (zz(2,3) /= 1) error stop 26
      if (zz(1,4) /= 2) error stop 27
      if (zz(2,4) /= 3) error stop 28

      end
