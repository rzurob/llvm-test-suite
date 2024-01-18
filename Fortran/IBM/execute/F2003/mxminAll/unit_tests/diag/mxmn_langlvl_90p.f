!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfixed -qdebug=intmsg -qlanglvl=90pure
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp mxmn_langlvl_90p.f 
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : mxmn_langlvl_90p 
!*
!*  PROGRAMMER                 : John Zang
!*  DATE                       : Oct. 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Support character argument for MAX/
!*                               MIN/MAXVAL/MINVAL/MAXLOC/MINLOC
!*  SECONDARY FUNCTIONS TESTED : Diagnostic - NOT allowed at language
!*                               level less than F2003 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : -qfixed -qdebug=intmsg -qlanglvl=90pure
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
      character(20) a, b, c, d, aa(10,5), bb(5,10), cc(3,2,3), dd(10)
      character(20) u, v, w(5), x(5), y(3,2), z
      a = 'IBM'
      b = 'Canada'
      c = 'Toronto'
      d = 'Markham'
      u = max(a, b)
      v = min(a, b)
      w = maxval(aa, dim = 1, mask = aa .ne. a)
      x = minval(bb, dim = 2, mask = bb .ne. b)
      y = maxval(cc, dim = 3, mask = cc .ne. c)
      z = minval(dd)
      end
