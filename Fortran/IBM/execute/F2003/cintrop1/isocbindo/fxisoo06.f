!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrison00.presh fxisoo06 cxisoo06
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
!*  KEYWORD(S)                 : C_LONG_DOUBLE_COMPLEX
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*
!*	- testing C_LONG_DOUBLE_COMPLEX
!*	- using external FORTRAN functions
!*	- passing 1-dim and 2-dim array arguments
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

complex(C_LONG_DOUBLE_COMPLEX) function fnt1(a)
   use ISO_C_BINDING

   complex(C_LONG_DOUBLE_COMPLEX) :: a(5)
   
   do i = 1, 5
      if ( a(i) /= cmplx(i,i,C_LONG_DOUBLE_COMPLEX) ) error stop 20
      a(i) = cmplx(i+1,i+1,C_LONG_DOUBLE_COMPLEX)
   end do

   fnt1 = 0
end function fnt1

complex(C_LONG_DOUBLE_COMPLEX) function fnt2(a)
   use ISO_C_BINDING

   complex(C_LONG_DOUBLE_COMPLEX), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,C_LONG_DOUBLE_COMPLEX) ) error stop 22
   end do

   fnt2 = 0
end function fnt2

complex(C_LONG_DOUBLE_COMPLEX) function fnt2a(a)
   use ISO_C_BINDING

   complex(C_LONG_DOUBLE_COMPLEX), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,C_LONG_DOUBLE_COMPLEX) ) error stop 24
   end do

   fnt2a = 0
end function fnt2a

complex(C_LONG_DOUBLE_COMPLEX) function fnt3(a)
   use ISO_C_BINDING

   complex(C_LONG_DOUBLE_COMPLEX), intent(inout) :: a(5)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,C_LONG_DOUBLE_COMPLEX) ) error stop 26
      a(i) = cmplx(i+1,i+1,C_LONG_DOUBLE_COMPLEX)
   end do

   fnt3 = 0
end function fnt3

complex(C_LONG_DOUBLE_COMPLEX) function fnt4(a)
   use ISO_C_BINDING

   complex(C_LONG_DOUBLE_COMPLEX), intent(out) :: a(5)

   do i = 1, 5
      a(i) = cmplx(i+1,i+1,C_LONG_DOUBLE_COMPLEX)
   end do

   fnt4 = 0
end function fnt4

complex(C_LONG_DOUBLE_COMPLEX) function fnt5(aa)
   use ISO_C_BINDING

   complex(C_LONG_DOUBLE_COMPLEX) :: aa(10,5)
   
   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,C_LONG_DOUBLE_COMPLEX) ) error stop 28
         aa(j,i) = cmplx(i+j,i+j,C_LONG_DOUBLE_COMPLEX)
      end do
   end do

   fnt5 = 0
end function fnt5

complex(C_LONG_DOUBLE_COMPLEX) function fnt6(aa)
   use ISO_C_BINDING

   complex(C_LONG_DOUBLE_COMPLEX), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,C_LONG_DOUBLE_COMPLEX) ) error stop 30
      end do
   end do

   fnt6 = 0
end function fnt6

complex(C_LONG_DOUBLE_COMPLEX) function fnt6a(aa)
   use ISO_C_BINDING

   complex(C_LONG_DOUBLE_COMPLEX), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,C_LONG_DOUBLE_COMPLEX) ) error stop 32
      end do
   end do

   fnt6a = 0
end function fnt6a

complex(C_LONG_DOUBLE_COMPLEX) function fnt7(aa)
   use ISO_C_BINDING

   complex(C_LONG_DOUBLE_COMPLEX), intent(inout) :: aa(10,5)
   
   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,C_LONG_DOUBLE_COMPLEX) ) error stop 34
         aa(j,i) = cmplx(i+j,i+j,C_LONG_DOUBLE_COMPLEX)
      end do
   end do

   fnt7 = 0
end function fnt7

complex(C_LONG_DOUBLE_COMPLEX) function fnt8(aa)
   use ISO_C_BINDING

   complex(C_LONG_DOUBLE_COMPLEX), intent(out) :: aa(10,5)
   
   do i = 1, 5
      do j = 1, 10
         aa(j,i) = cmplx(i+j,i+j,C_LONG_DOUBLE_COMPLEX)
      end do
   end do

   fnt8 = 0
end function fnt8
