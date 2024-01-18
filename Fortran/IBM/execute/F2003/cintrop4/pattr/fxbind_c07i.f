! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c07i bind_c07i
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
!* DATE                         : Sep. 1, 2003
!* ORIGIN                       : AIX Complier Development
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test: BINC(C) attribute function
!*                                with different intrinsic data type,
!*                                integer*1, integer*2, integer*4,
!*                                integer*8, real*4, real*8, complex
!*                                character(1). Using external
!*                                function,interface. C calls Fortran.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890


       function exfun_int1(a1) result(a2) BIND(C)
            integer*1, value :: a1
            integer*1 a2
            a2 = a1 + 3
       end function exfun_int1

       function exfun_int2(b1) result(b2) BIND(C)
             integer*2 , value :: b1
             integer*2 b2
             b2 = b1 + 3
       end function exfun_int2

       function exfun_int4(c1) result(c2) BIND(C)
             integer*4 , value :: c1
             integer*4 c2
             c2 = c1 + 3
       end function exfun_int4

       function exfun_int8(d1) result(d2) BIND(C)
            integer*8 , value :: d1
            integer*8 d2
            d2 = d1 + 3
       end function exfun_int8

       function exfun_real4(e1) result(e2) BIND(C)
            real*4 , value :: e1
            real*4 e2
            e2 = e1 * 2
       end function exfun_real4

       function exfun_real8(f1) result(f2) BIND(C)
            real*8 , value :: f1
            real*8 f2
            f2 = f1 * 2
       end function exfun_real8

       function exfun_real16(g1) result(g2) BIND(C)
             real*16 , value :: g1
             real*16 g2
             g2 = g1 * 2
       end function exfun_real16

       function exfun_log1(h1) result(h2) BIND(C)
          logical*1 , value :: h1
          logical*1 h2
          h2 = .not. h1
       end function exfun_log1

       function exfun_char(n1) result(n2) BIND(C)
            character*1 , value :: n1
            character*1 n2
            n2 = n1
       end function exfun_char

       function exfun_comp1(l1) result(l2) bind(c)
            complex*8, value :: l1
            complex*8 l2
            l2 = l1 + (1.0,1.0)
       end function exfun_comp1

       function exfun_comp2(m1) result(m2) bind(c)
            complex*16, value :: m1
            complex*16 m2
            m2 = m1 + (1.0D0, 1.0D0)
       end function exfun_comp2


