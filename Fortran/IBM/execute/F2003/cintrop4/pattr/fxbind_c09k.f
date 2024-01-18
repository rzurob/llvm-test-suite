! *********************************************************************
!**********************************************************************
!* ===================================================================
!*
!* DATE                         : Sep. 1, 2003
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test: BINC(C) attribute/statement
!*                                with different intrinsic data type,
!*                                integer*1, integer*2, integer*4,
!*                                integer*8, real*4, real*8, real*16,
!*                                byte, character(1). Using external
!*                                recursive function entry.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890


       recursive function sexfun_int1(a1) result(a2)
            integer*1 :: a1
            integer*1 a2
            a2 = -1
            return
       entry exfun_int1(a1) result(a2) bind(c)
            if ( a1 > 0 ) then
              a1 = a1 - 1
              a2 = 2 + exfun_int1(a1)
            else
              a2 = a1
            end if
       end function sexfun_int1

       recursive function sexfun_int2(b1) result(b2)
             integer*2 :: b1
             integer*2 b2
             b2 = -1
             return
       entry exfun_int2(b1) result(b2) bind(c)
             if ( b1 > 0 ) then
               b1 = b1 - 1
               b2 = 2 + exfun_int2(b1)
             else
               b2 = b1
             end if
       end function sexfun_int2

       recursive function sexfun_int4(c1) result(c2)
             integer*4  :: c1
             integer*4 c2
             c2 = -1
             return
       entry exfun_int4(c1) result(c2) bind(c)
             if ( c1 > 0 ) then
                c1 = c1 - 1
                c2 = 2 + exfun_int4(c1)
             else
                c2 = c1
             end if
       end function sexfun_int4

       recursive function sexfun_int8(d1) result(d2)
            integer*8  :: d1
            integer*8 d2
            d2 = -1
            return
       entry exfun_int8(d1) result(d2) bind(c)
            if ( d1 > 0 ) then
              d1 = d1 - 1
              d2 = 2 + exfun_int8(d1)
            else
              d2 = d1
            end if
       end function sexfun_int8

       recursive function sexfun_real4(e1,i) result(e2)
            real*4 :: e1
            integer i
            real*4 e2
            e2 = -10.0
            return
      entry exfun_real4(e1, i) result(e2) bind(c)
            if ( i > 0 ) then
              i = i - 1
              e2 = e1 + exfun_real4(e1, i)
            else
              e2 = e1
            end if
       end function sexfun_real4

       recursive function sexfun_real8(f1, i) result(f2)
            real*8  :: f1
            integer i
            real*8 f2
            f2 = -10.0
            return
        entry exfun_real8(f1, i) result(f2) bind(c)
            if ( i > 0 ) then
              i = i - 1
              f2 = f1 + exfun_real8(f1, i)
            else
              f2 = f1
            end if
       end function sexfun_real8


       recursive function sexfun_comp1(l1, i) result(l2)
            complex*8 :: l1
            integer i
            complex*8 l2
            l2 = (-1.0, -2.0)
            return
       entry exfun_comp1(l1, i) result(l2) bind(c)
            if ( i > 0) then
              i = i - 1
              l2 = (1.0, 2.0) + exfun_comp1(l1,i)
            else
              l2 = l1
            end if
       end function sexfun_comp1

       recursive function sexfun_comp2(m1, i) result(m2)
            complex*16 :: m1
            integer i
            complex*16 m2
            m2 = (-1.0D0, -2.0D0)
            return
       entry exfun_comp2(m1, i) result(m2) bind(c)
            if ( i > 0 ) then
              i = i - 1
              m2 = (1.0d0, 2.0D0) + exfun_comp2(m1, i)
            else
              m2 = m1
            end if
       end function sexfun_comp2

