! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c05d bind_c01g
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan. 1, 2004
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test: BINC(C) attribute
!*                                with different intrinsic data type,
!*                                integer*1, integer*2, integer*4,
!*                                integer*8, real*4, real*8, complex
!*                                character(1). Using module
!*                                subroutine. C calls Fortran. With
!*                                implicit typing
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
contains
       subroutine extsub_int(a, b, c, d) BIND(C)
           implicit integer*1 (a)
           implicit integer*2 (b)
           implicit integer*4 (c)
           implicit integer*8 (d)
           a = a + 3
           b = b + 3
           c = c + 3
           d = d + 3
       end subroutine extsub_int

       subroutine extsub_real(e, f) BIND(C)
           implicit real*4  (e)
           implicit real*8  (f)
           e = e * 2
           f = f * 2
       end subroutine extsub_real

       subroutine extsub_log(h) BIND(C)
           implicit logical*1 (h)
           h = .true.
       end subroutine extsub_log

       subroutine extsub_comp(l, m) BIND(C)
           implicit complex*8   (l)
           implicit complex*16  (m)
           l = (1.0, 1.0)
           m = (1.0D0, 1.0D0)
       end subroutine extsub_comp
end module m
       subroutine extsub_char(n) BIND(C)
           implicit character*1 (n)
           n = 'd'
       end subroutine extsub_char


