! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: EXEC_REP=1; $TR_SRC/fxbindc.sh fxbind_c05b bind_c01a
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
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : fxbind_c05b.f
!* TEST CASE TITLE              : BIND(C) attribute/statement
!*
!* PROGRAMMER                   : Yubin Liao
!* DATE                         : Sep. 1, 2003
!* ORIGIN                       : AIX Complier Development
!*                              : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     :
!* SECONDARY FUNTIONS TESTED
!*
!* DRIVER STANZA                : xlf90
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test: BINC(C) attribute/statement 
!*                                with different intrinsic data type,
!*                                integer*1, integer*2, integer*4,
!*                                integer*8, real*4, real*8, complex
!*                                character(1). Using module 
!*                                subroutine,interface.Fortran calls C.
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  03/05/03    SK     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m

   interface extsub
       subroutine extsub_int(a, b, c, d) bind(c) 
           implicit integer*1 (a)
           implicit integer*2 (b)
           implicit integer*4 (c)
           implicit integer*8 (d)
       end subroutine extsub_int

       subroutine extsub_real(e, f) bind(c) 
           implicit real*4 (e)
           implicit real*8 (f)
 
       end subroutine extsub_real

       subroutine extsub_log(h) bind(c) 
           implicit logical*1 (h)
          
       end subroutine extsub_log

       subroutine extsub_comp(l, m) bind(c) 
           implicit complex*8  (l) 
           implicit complex*16 (m) 
       end subroutine extsub_comp

       subroutine extsub_char(n) bind(c) 
           implicit character*1 (n)
       end subroutine extsub_char

       
   end interface

end module m

use m

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

 
   call extsub(a1, b1, c1, d1)
      if(a1 .ne. a2)then
        error stop 10
      endif

      if(b1 .ne. b2)then
        error stop 11
      endif

      if(c1 .ne. c2)then
        error stop 12
      endif

      if(d1 .ne. d2)then
        error stop 13
      endif

   call extsub(e1, f1)

      if(.not. precision_R4(e1,e2))then
        error stop 20
      endif

      if(.not. precision_R8(f1,f2))then
        error stop 21
      endif


   call extsub(h1)
      if(h1 .neqv. h2)then
        error stop 30
      endif


   call extsub(n1)
      if(n1 .ne. n2)then
        error stop 50
      endif
   
   
   call extsub(l1, m1)
   
      if(.not. precision_x8(l1, l2))then
        error stop 61
      endif

      if(.not. precision_x16(m1, m2))then
        error stop 62
      endif

end 
