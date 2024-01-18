! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c05g bind_c05g
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
!* DATE                         : Jan. 1, 2003
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
!*                                integer*8, real*4, real*8, complex
!*                                character(1). Using external
!*                                function,interface. Fortran calls C.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxbind_c05g

   interface
       function exfun_int1(a1) result(a2) BIND(C)
           implicit integer*1 (a)
       end function exfun_int1

       function exfun_int2(b1) result(b2) BIND(C)
            implicit integer*2 (b)
       end function exfun_int2

       function exfun_int4(c1) result(c2) BIND(C)
            implicit integer*4 (c)
       end function exfun_int4

       function exfun_int8(d1) result(d2) BIND(C)
           implicit integer*8 (d)
       end function exfun_int8

       function exfun_real4(e1) result(e2) BIND(C)
            implicit real*4 (e)
       end function exfun_real4

       function exfun_real8(f1) result(f2) BIND(C)
           implicit real*8 (f)
       end function exfun_real8

       function exfun_real16(g1) result(g2) BIND(C)
            implicit real*16 (g)
       end function exfun_real16

       function exfun_log1(h1) result(h2) BIND(C)
           implicit logical*1 (h)
       end function exfun_log1

       function exfun_char(n1) result(n2) BIND(C)
           implicit character*1 (n)
       end function exfun_char

       function exfun_comp1(l1) result(l2) bind(c)
           implicit complex*8 (l)
       end function exfun_comp1

       function exfun_comp2(m1) result(m2) bind(c)
           implicit complex*16 (m)
       end function exfun_comp2

   end interface

   logical precision_R4, precision_R6, precision_R8
   logical precision_x8, precision_x16

   implicit integer*1 (a)
   implicit integer*2 (b)
   implicit integer*4 (c)
   implicit integer*8 (d)

   implicit real*4   (e)
   implicit real*8   (f)
   implicit real*16  (g)

   implicit logical*1 (h)


   implicit character*1 (n)

   implicit complex*8 (l)
   implicit complex*16 (m)

   a1 = 5
   a2 = 8

   b1 = 15
   b2 = 18

   c1 = 11
   c2 = 14

   d1 = 17
   d2 = 20

   e1 = 4.80
   e2 = 9.6

   f1 = 140.8
   f2 = 281.6

   g1 = 1600.3
   g2 = 3200.6

   h1 = .false.
   h2 = .true.


   n1 = 'a'
   n2 = 'd'

   l1 = (0.0, 0.0)
   l2 = (1.0, 1.0)

   m1 = (0.0D0, 0.0D0)
   m2 = (1.0D0, 1.0D0)

    a3 = exfun_int1(a1)
      if(a3 .ne. a2)then
        error stop 10
      endif

    b3 = exfun_int2(b1)
      if(b3 .ne. b2)then
        error stop 11
      endif

    c3 = exfun_int4(c1)
      if(c3 .ne. c2)then
        error stop 12
      endif

    d3 = exfun_int8(d1)
      if(d3 .ne. d2)then
        error stop 13
      endif

    e3 = exfun_real4(e1)
      if(.not. precision_R4(e3,e2))then
        error stop 20
      endif

    f3 = exfun_real8(f1)
      if(.not. precision_R8(f3,f2))then
        error stop 21
      endif


    h3 = exfun_log1(h1)
      if(h3 .neqv. h2)then
        error stop 30
      endif



    n3 = exfun_char(n1)
    print *, n3
      if(n3 .ne. n2)then
        error stop 40
      endif

    l3 = exfun_comp1(l1)
      if(.not. precision_x8(l3,l2))then
        error stop 50
      end if

    m3 = exfun_comp2(m1)
      if(.not. precision_x16(m3,m2))then
        error stop 51
      end if

end
