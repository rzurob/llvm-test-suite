! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c11g bind_c11g
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
!* DESCRIPTION                  : Test: BINC(C) attribute/statement
!*                                with different intrinsic data type
!*                                and attribute intent and target
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
            integer*1, intent(in), target :: a1
            integer*1 ::  a2
       end function exfun_int1

       function exfun_int2(b1) result(b2) BIND(C)
             integer*2 ,intent(in), target :: b1
             integer*2 b2
       end function exfun_int2

       function exfun_int4(c1) result(c2) BIND(C)
             integer*4 ,intent(in), target :: c1
             integer*4 c2
       end function exfun_int4

       function exfun_int8(d1) result(d2) BIND(C)
            integer*8 ,intent(in), target :: d1
            integer*8 d2
       end function exfun_int8

       function exfun_real4(e1) result(e2) BIND(C)
            real*4 , intent(in), target :: e1
            real*4 e2
       end function exfun_real4

       function exfun_real8(f1) result(f2) BIND(C)
            real*8 ,intent(in), target :: f1
            real*8 f2
       end function exfun_real8


       function exfun_log1(h1) result(h2) BIND(C)
          logical*1 ,intent(in), target :: h1
          logical*1 h2
       end function exfun_log1

       function exfun_char(n1) result(n2) BIND(C)
            character*1 ,intent(in), target  :: n1
            character*1 n2
       end function exfun_char

       function exfun_comp1(l1) result(l2) bind(c)
            complex*8, intent(in), target  :: l1
            complex*8 l2
       end function exfun_comp1

       function exfun_comp2(m1) result(m2) bind(c)
            complex*16, intent(in), target :: m1
            complex*16 m2
       end function exfun_comp2

   end interface

   logical precision_R4, precision_R6, precision_R8
   logical precision_x8, precision_x16



   integer*1 a1 /5/, a3 /0/
   integer*1 a2 /8/, a4 /5/

   integer*2 b1 /15/, b3 /0/
   integer*2 b2 /18/, b4 /15/

   integer*4 c1 /11/, c3 /0/
   integer*4 c2 /14/, c4 /11/

   integer*8 d1 /17/, d3 /0/
   integer*8 d2 /20/, d4 /17/

   real*4 e1 /4.80/, e3 /0.0/
   real*4 e2 /9.6/, e4 /4.80/

   real*8 f1 /140.8D0/, f3
   real*8 f2 /281.6D0/, f4/140.8D0/


   logical*1 h1 /.false./, h3
   logical*1 h2 /.true./, h4 /.false./


   character*1 n1 /'a'/, n3
   character*1 n2 /'a'/, n4 /'a'/

   complex*8 l1 /(0.0, 0.0)/, l3
   complex*8 l2 /(1.0, 1.0)/, l4 /(0.0, 0.0)/

   complex*16 m1 /(0.0D0, 0.0D0)/, m3
   complex*16 m2 /(1.0D0, 1.0D0)/, m4 /(0.0D0, 0.0D0)/

    a3 = exfun_int1(a1)
      if(a3 .ne. a2)then
        error stop 10
      endif
      if(a1 .ne. a4) then
        error stop 110
      end if

    b3 = exfun_int2(b1)
      if(b3 .ne. b2) then
        error stop 11
      endif
      if(b1 .ne. b4) then
        error stop 111
      end if

    c3 = exfun_int4(c1)
      if(c3 .ne. c2) then
        error stop 12
      endif
      if (c1 .ne. c4) then
        error stop 112
      end if

    d3 = exfun_int8(d1)
      if(d3 .ne. d2)then
        error stop 13
      endif
      if (d1 .ne. d4) then
        error stop 113
      end if

    e3 = exfun_real4(e1)
      if(.not. precision_R4(e3,e2))then
        error stop 20
      endif
      if ( .not. precision_r4(e1, e4)) then
        error stop 120
      end if

    f3 = exfun_real8(f1)
      if(.not. precision_R8(f3,f2))then
        error stop 21
      endif
      if (.not. precision_r8(f1, f4)) then
        error stop 121
      end if


    h3 = exfun_log1(h1)
      if(h3 .neqv. h2)then
        error stop 30
      endif
      if (h1 .neqv. h4) then
        error stop 130
      end if


    n3 = exfun_char(n1)
      if(n3 .ne. n2)then
        error stop 40
      endif
      if (n1 .ne. n4) then
        error stop 140
      end if


    l3 = exfun_comp1(l1)
      if(.not. precision_x8(l3,l2))then
        error stop 50
      end if
      if(.not. precision_x8(l1, l4)) then
        error stop 150
      end if

    m3 = exfun_comp2(m1)
      if(.not. precision_x16(m3,m2))then
        error stop 51
      end if
      if(.not. precision_x16(m1, m4)) then
        error stop 151
      end if

end
