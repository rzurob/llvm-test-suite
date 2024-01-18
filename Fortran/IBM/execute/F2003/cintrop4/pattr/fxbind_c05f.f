! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Jan. 1, 2004
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
!*                                entry. C calls Fortran. With
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
       subroutine sextsub_int(a, b, c, d)
           implicit integer*1 (a)
           implicit integer*2 (b)
           implicit integer*4 (c)
           implicit integer*8 (d)
           return
      entry extsub_int(a, b, c, d) bind(c)
           a = a + 3
           b = b + 3
           c = c + 3
           d = d + 3
       end subroutine sextsub_int

       subroutine sextsub_real(e, f) BIND(C)
           implicit real*4  (e)
           implicit real*8  (f)
           return
       entry extsub_real(e, f) bind(c)
           e = e * 2
           f = f * 2
       end subroutine sextsub_real

       subroutine sextsub_log(h) BIND(C)
           implicit logical*1 (h)
           return
       entry extsub_log(h) bind(c)
           h = .true.
       end subroutine sextsub_log

       subroutine sextsub_comp(l, m) BIND(C)
           implicit complex*8   (l)
           implicit complex*16  (m)
           return
       entry extsub_comp(l, m) bind(c)
           l = (1.0, 1.0)
           m = (1.0D0, 1.0D0)
       end subroutine sextsub_comp

       subroutine sextsub_char(n)
           implicit character*1 (n)
           return
       entry extsub_char(n) bind(c)
           n = 'd'
       end subroutine sextsub_char

end module m
