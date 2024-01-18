!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrison00.presh fxison06 cxison06
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************
!***********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Support for ISO_C_BINDING module
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquide
!*  DATE                       : 4/23/2002
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*
!*  PRIMARY FUNCTIONS TESTED   : ISO_C_BINDING module
!*  SECONDARY FUNCTIONS TESTED : see below 
!*
!*  DRIVER STANZA              : 
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : C_FLOAT_COMPLEX, C_DOUBLE_COMPLEX
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*
!*	- testing C_FLOAT_COMPLEX and C_DOUBLE_COMPLEX
!*	- using external FORTRAN functions
!*	- passing 1-dim and 2-dim array arguments
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

complex(C_FLOAT_COMPLEX) function fnt1(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX) :: a(5)
   complex(C_DOUBLE_COMPLEX) :: b(5)
   
   do i = 1, 5
      if ( a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 20
      a(i) = cmplx(i+1,i+1,C_FLOAT_COMPLEX)
      if ( b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 22
      b(i) = cmplx(i+1,i+1,C_DOUBLE_COMPLEX)
   end do

   fnt1 = 0
end function fnt1

complex(C_FLOAT_COMPLEX) function fnt2(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(in) :: a(5)
   complex(C_DOUBLE_COMPLEX), intent(in) :: b(5)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 24
      if ( b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 26
   end do

   fnt2 = 0
end function fnt2

complex(C_FLOAT_COMPLEX) function fnt2a(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(in) :: a(5)
   complex(C_DOUBLE_COMPLEX), intent(in) :: b(5)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 28
      if ( b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 30
   end do

   fnt2a = 0
end function fnt2a

complex(C_FLOAT_COMPLEX) function fnt3(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(inout) :: a(5)
   complex(C_DOUBLE_COMPLEX), intent(inout) :: b(5)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 32
      a(i) = cmplx(i+1,i+1,C_FLOAT_COMPLEX)
      if ( b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 34
      b(i) = cmplx(i+1,i+1,C_DOUBLE_COMPLEX)
   end do

   fnt3 = 0
end function fnt3

complex(C_FLOAT_COMPLEX) function fnt4(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(out) :: a(5)
   complex(C_DOUBLE_COMPLEX), intent(out) :: b(5)

   do i = 1, 5
      a(i) = cmplx(i+1,i+1,C_FLOAT_COMPLEX)
      b(i) = cmplx(i+1,i+1,C_DOUBLE_COMPLEX)
   end do

   fnt4 = 0
end function fnt4

complex(C_FLOAT_COMPLEX) function fnt5(aa,bb)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX) :: aa(10,5)
   complex(C_DOUBLE_COMPLEX) :: bb(10,5)
   
   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,C_FLOAT_COMPLEX) ) error stop 36
         aa(j,i) = cmplx(i+j,i+j,C_FLOAT_COMPLEX)
         if ( bb(j,i) /= cmplx(i+j-1,i+j-1,C_DOUBLE_COMPLEX) ) error stop 38
         bb(j,i) = cmplx(i+j,i+j,C_DOUBLE_COMPLEX)
      end do
   end do

   fnt5 = 0
end function fnt5

complex(C_FLOAT_COMPLEX) function fnt6(aa,bb)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(in) :: aa(10,5)
   complex(C_DOUBLE_COMPLEX), intent(in) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,C_FLOAT_COMPLEX) ) error stop 40
         if ( bb(j,i) /= cmplx(i+j-1,i+j-1,C_DOUBLE_COMPLEX) ) error stop 42
      end do
   end do

   fnt6 = 0
end function fnt6

complex(C_FLOAT_COMPLEX) function fnt6a(aa,bb)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(in) :: aa(10,5)
   complex(C_DOUBLE_COMPLEX), intent(in) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,C_FLOAT_COMPLEX) ) error stop 44
         if ( bb(j,i) /= cmplx(i+j-1,i+j-1,C_DOUBLE_COMPLEX) ) error stop 46
      end do
   end do

   fnt6a = 0
end function fnt6a

complex(C_FLOAT_COMPLEX) function fnt7(aa,bb)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(inout) :: aa(10,5)
   complex(C_DOUBLE_COMPLEX), intent(inout) :: bb(10,5)
   
   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,C_FLOAT_COMPLEX) ) error stop 48
         aa(j,i) = cmplx(i+j,i+j,C_FLOAT_COMPLEX)
         if ( bb(j,i) /= cmplx(i+j-1,i+j-1,C_DOUBLE_COMPLEX) ) error stop 50
         bb(j,i) = cmplx(i+j,i+j,C_DOUBLE_COMPLEX)
      end do
   end do

   fnt7 = 0
end function fnt7

complex(C_FLOAT_COMPLEX) function fnt8(aa,bb)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(out) :: aa(10,5)
   complex(C_DOUBLE_COMPLEX), intent(out) :: bb(10,5)
   
   do i = 1, 5
      do j = 1, 10
         aa(j,i) = cmplx(i+j,i+j,C_FLOAT_COMPLEX)
         bb(j,i) = cmplx(i+j,i+j,C_DOUBLE_COMPLEX)
      end do
   end do

   fnt8 = 0
end function fnt8
