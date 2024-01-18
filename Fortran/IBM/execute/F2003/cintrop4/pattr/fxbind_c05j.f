! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c05j bind_c05i
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
!*                                function,interface. Fortran calls C.
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
       function exfun_int1(a1) result(a2) BIND(C)
           implicit integer*1 (a)
           a2 = a1 + 3
       end function exfun_int1

       function exfun_int2(b1) result(b2) BIND(C)
            implicit integer*2 (b)
            b2 = b1 + 3
       end function exfun_int2

       function exfun_int4(c1) result(c2) BIND(C)
            implicit integer*4 (c)
            c2 = c1 + 3
       end function exfun_int4

       function exfun_int8(d1) result(d2) BIND(C)
           implicit integer*8 (d)
           d2 = d1 + 3
       end function exfun_int8

       function exfun_real4(e1) result(e2) BIND(C)
            implicit real*4 (e)
            e2 = e1 * 2
       end function exfun_real4

       function exfun_real8(f1) result(f2) BIND(C)
           implicit real*8 (f)
           f2 = f1 * 2
       end function exfun_real8

       function exfun_real16(g1) result(g2) BIND(C)
            implicit real*16 (g)
            g2 = g1 * 2
       end function exfun_real16

       function exfun_log1(h1) result(h2) BIND(C)
           implicit logical*1 (h)
           h2 = .not. h1
       end function exfun_log1

       function exfun_char(n1) result(n2) BIND(C)
           implicit character*1 (n)
           n2 = 'd'
       end function exfun_char

       function exfun_comp1(l1) result(l2) bind(c)
           implicit complex*8 (l)
           l2 = l1 + (1.0, 1.0)
       end function exfun_comp1

       function exfun_comp2(m1) result(m2) bind(c)
           implicit complex*16 (m)
           m2 = m1 + (1.0D0, 1.0D0)
       end function exfun_comp2

 end module m



