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
!* DESCRIPTION                  : Test: BINC(C) attribute/statement
!*                                with different intrinsic data type
!*                                with attribute intent and target,
!*                                integer*1, integer*2, integer*4,
!*                                integer*8, real*4, real*8, complex
!*                                character(1). Using Fortran implemented
!*                                module entry.
!*
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
            integer*1, intent(in), target :: a1
            integer*1 a2
            a2 = a1
            return
       entry exfun_int1(a1) result(a2) bind(c)
            a2 = a1 + 3
       end function fexfun_int1

       function fexfun_int2(b1) result(b2)
             integer*2 , target, intent(in)  :: b1
             integer*2 b2
             b2 = b1
             return
       entry exfun_int2(b1) result(b2) bind(c)
             b2 = b1 + 3
       end function fexfun_int2

       function fexfun_int4(c1) result(c2)
             integer*4 , intent(in), target :: c1
             integer*4 c2
             c2 = c1
             return
       entry exfun_int4(c1) result(c2) bind(c)
             c2 = c1 + 3
       end function fexfun_int4

       function fexfun_int8(d1) result(d2)
            integer*8 ,intent(in), target  :: d1
            integer*8 d2
            d2 = d1
            return
       entry exfun_int8(d1) result(d2) bind(c)
            d2 = d1 + 3
       end function fexfun_int8

       function fexfun_real4(e1) result(e2)
            real*4 ,intent(in), target  :: e1
            real*4 e2
            e2 = e1
            return
       entry exfun_real4(e1) result(e2) bind(c)
            e2 = e1 * 2
       end function fexfun_real4

       function fexfun_real8(f1) result(f2)
            real*8 ,intent(in), target  :: f1
            real*8 f2
            f2 = f1
            return
       entry exfun_real8(f1) result(f2) bind(c)
            f2 = f1 * 2
       end function fexfun_real8

       function fexfun_log1(h1) result(h2)
          logical*1 ,intent(in), target  :: h1
          logical*1 h2
          h2 = h1
          return
       entry exfun_log1(h1) result(h2) bind(c)
          h2 = .not. h1
       end function fexfun_log1

       function fexfun_comp1(l1) result(l2)
            complex*8, intent(in), target :: l1
            complex*8 l2
            l2 = l1
            return
       entry exfun_comp1(l1) result(l2) bind(c)
            l2 = l1 + (1.0,1.0)
       end function fexfun_comp1

       function fexfun_comp2(m1) result(m2)
            complex*16,intent(in), target  :: m1
            complex*16 m2
            me = m1
            return
       entry exfun_comp2(m1) result(m2) bind(c)
            m2 = m1 + (1.0D0, 1.0D0)
       end function fexfun_comp2

 end module m
