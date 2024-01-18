!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfixed -qdebug=intmsg
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp mxmn_type_conflict.f
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Support character argument for MAX/
!*                               MIN/MAXVAL/MINVAL/MAXLOC/MINLOC
!*  SECONDARY FUNCTIONS TESTED : Diagnostic - NOT allowed at language
!*                               level less than F2003
!*
!*  REQUIRED COMPILER OPTIONS  : -qfixed -qdebug=intmsg
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
      character(20) a, b, aa(10,5), bb(10,5)
      integer c, d, cc(10,5), dd(5,10)
      real e, f, ee(10,5), ff(5,10)
      a = max(b, c, e)
      a = min(b, c, e)
      c = max(b, e, c)
      c = min(b, e, c)
      f = max(b, c, e)
      f = min(b, c, e)
      f = max(a, b)
      f = min(a, b)
      aa = max(aa, cc)
      aa = min(aa, cc)
      aa = max(aa, ee)
      aa = min(aa, ee)
      ee = max(aa, bb)
      ee = min(aa, bb)
      end
