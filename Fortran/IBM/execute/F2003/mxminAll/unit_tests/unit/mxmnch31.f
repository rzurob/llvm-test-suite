!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfixed
! %GROUP: mxmnch31.f
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
      integer i, xx(1), yy(2)
      character(10) aa(500000), bb(100,100)

      aa = '!'
      aa(283737) = 'maxval'
      xx = maxloc(aa)
      if (xx(1) /= 283737) error stop 1

      aa = '~'
      aa(284363) = 'minval'
      xx = minloc(aa)
      if (xx(1) /= 284363) error stop 2

      bb = '!'
      bb(87, 34) = '8734'
      bb(3, 92) = '0392'
      yy = maxloc(bb)
      if (yy(1) /= 87) error stop 3
      if (yy(2) /= 34) error stop 4

      bb = '~'
      bb(87, 34) = '8734'
      bb(3, 92) = '0392'
      yy = minloc(bb)
      if (yy(1) /= 3) error stop 5
      if (yy(2) /= 92) error stop 6
      end
