! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c05l bind_c05i
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
!* DESCRIPTION                  : Test: BINC(C) attribute/statement
!*                                with different intrinsic data type,
!*                                integer*1, integer*2, integer*4,
!*                                integer*8, real*4, real*8, complex,
!*                                character(1). Using module
!*                                entry. Fortran calls C.
!*                                with implicit typing
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

       function fexfun_int1(a1) result(a2)
           implicit integer*1 (a)
           a2 = a1
           return
       entry exfun_int1(a1) result(a2) bind(c)
           a2 = a1 + 3
       end function fexfun_int1

       function fexfun_int2(b1) result(b2)
            implicit integer*2 (b)
            b2 = b1
            return
       entry exfun_int2(b1) result(b2) bind(c)
            b2 = b1 + 3
       end function fexfun_int2

       function fexfun_int4(c1) result(c2)
            implicit integer*4 (c)
            c2 = c1
            return
       entry exfun_int4(c1) result(c2) bind(c)
            c2 = c1 + 3
       end function fexfun_int4

       function fexfun_int8(d1) result(d2)
           implicit integer*8 (d)
           d2 = d1
           return
       entry exfun_int8(d1) result(d2) bind(c)
           d2 = d1 + 3
       end function fexfun_int8

       function fexfun_real4(e1) result(e2)
            implicit real*4 (e)
            e2 = e1
            return
       entry exfun_real4(e1) result(e2) bind(c)
            e2 = e1 * 2
       end function fexfun_real4

       function fexfun_real8(f1) result(f2)
           implicit real*8 (f)
           f2 = f1
           return
       entry exfun_real8(f1) result(f2) bind(c)
           f2 = f1 * 2
       end function fexfun_real8

       function fexfun_real16(g1) result(g2)
            implicit real*16 (g)
            g2 = g1
            return
       entry exgun_real16(g1) result(g2) bind(c)
            g2 = g1 * 2
       end function fexfun_real16

       function fexfun_log1(h1) result(h2)
           implicit logical*1 (h)
           h2 = h1
           return
       entry exfun_log1(h1) result(h2) bind(c)
           h2 = .not. h1
       end function fexfun_log1

       function fexfun_comp1(l1) result(l2)
           implicit complex*8 (l)
           l2 = l1
           return
       entry exfun_comp1(l1) result(l2) bind(c)
           l2 = l1 + (1.0, 1.0)
       end function fexfun_comp1

       function fexfun_comp2(m1) result(m2)
           implicit complex*16 (m)
           m2 = m1
           return
       entry exfun_comp2(m1) result(m2) bind(c)
           m2 = m1 + (1.0D0, 1.0D0)
       end function fexfun_comp2


end module m

