!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrison00.presh fxison08 cxison08
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 4/23/2002
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ISO_C_BINDING module
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : C_FLOAT_COMPLEX, C_DOUBLE_COMPLEX
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_FLOAT_COMPLEX and C_DOUBLE_COMPLEX
!*      - using C functions with interface to FORTRAN functions
!*      - passing 1-dim and 2-dim array arguments
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxison08
   use ISO_C_BINDING

   interface
      complex(C_FLOAT_COMPLEX) function fnt1(a,b)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX) :: a(5)
         complex(C_DOUBLE_COMPLEX) :: b(5)
      end function fnt1
      complex(C_FLOAT_COMPLEX) function fnt2(a,b)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX), intent(in) :: a(5)
         complex(C_DOUBLE_COMPLEX), intent(in) :: b(5)
      end function fnt2
      complex(C_FLOAT_COMPLEX) function fnt2a(a,b)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX), intent(in) :: a(5)
         complex(C_DOUBLE_COMPLEX), intent(in) :: b(5)
      end function fnt2a
      complex(C_FLOAT_COMPLEX) function fnt3(a,b)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX), intent(inout) :: a(5)
         complex(C_DOUBLE_COMPLEX), intent(inout) :: b(5)
      end function fnt3
      complex(C_FLOAT_COMPLEX) function fnt4(a,b)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX), intent(out) :: a(5)
         complex(C_DOUBLE_COMPLEX), intent(out) :: b(5)
      end function fnt4
      complex(C_FLOAT_COMPLEX) function fnt5(aa,bb)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX) :: aa(10,5)
         complex(C_DOUBLE_COMPLEX) :: bb(10,5)
      end function fnt5
      complex(C_FLOAT_COMPLEX) function fnt6(aa,bb)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX), intent(in) :: aa(10,5)
         complex(C_DOUBLE_COMPLEX), intent(in) :: bb(10,5)
      end function fnt6
      complex(C_FLOAT_COMPLEX) function fnt6a(aa,bb)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX), intent(in) :: aa(10,5)
         complex(C_DOUBLE_COMPLEX), intent(in) :: bb(10,5)
      end function fnt6a
      complex(C_FLOAT_COMPLEX) function fnt7(aa,bb)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX), intent(inout) :: aa(10,5)
         complex(C_DOUBLE_COMPLEX), intent(inout) :: bb(10,5)
      end function fnt7
      complex(C_FLOAT_COMPLEX) function fnt8(aa,bb)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX), intent(out) :: aa(10,5)
         complex(C_DOUBLE_COMPLEX), intent(out) :: bb(10,5)
      end function fnt8
   end interface

   complex(C_FLOAT_COMPLEX) :: a(5), aa(10,5), ret
   complex(C_DOUBLE_COMPLEX) :: b(5), bb(10,5)
   integer i, j

!! Test 1

   call initarr1d(a,b)

   ret = fnt1(a,b)

   do i = 1, 5
      if ( a(i) /= cmplx(i+1,i+1,C_FLOAT_COMPLEX) ) error stop 20
      if ( b(i) /= cmplx(i+1,i+1,C_DOUBLE_COMPLEX) ) error stop 22
   end do

!! Test 2

   call initarr1d(a,b)

   ret = fnt2(a,b)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 24
      if ( b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 26
   end do

!! Test 2a

   call initarr1d(a,b)

   ret = fnt2a(a,b)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 28
      if ( b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 30
   end do

!! Test 3

   call initarr1d(a,b)

   ret = fnt3(a,b)

   do i = 1, 5
      if ( a(i) /= cmplx(i+1,i+1,C_FLOAT_COMPLEX) ) error stop 32
      if ( b(i) /= cmplx(i+1,i+1,C_DOUBLE_COMPLEX) ) error stop 34
   end do

!! Test 4

   call initarr1d(a,b)

   ret = fnt4(a,b)

   do i = 1, 5
      if ( a(i) /= cmplx(i+1,i+1,C_FLOAT_COMPLEX) ) error stop 36
      if ( b(i) /= cmplx(i+1,i+1,C_DOUBLE_COMPLEX) ) error stop 38
   end do

!! Test 5

   call initarr2d(aa,bb)

   ret = fnt5(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j,i+j,C_FLOAT_COMPLEX) ) error stop 40
         if ( bb(j,i) /= cmplx(i+j,i+j,C_DOUBLE_COMPLEX) ) error stop 42
      end do
   end do

!! Test 6

   call initarr2d(aa,bb)

   ret = fnt6(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,C_FLOAT_COMPLEX) ) error stop 44
         if ( bb(j,i) /= cmplx(i+j-1,i+j-1,C_DOUBLE_COMPLEX) ) error stop 46
      end do
   end do

!! Test 6a

   call initarr2d(aa,bb)

   ret = fnt6a(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,C_FLOAT_COMPLEX) ) error stop 48
         if ( bb(j,i) /= cmplx(i+j-1,i+j-1,C_DOUBLE_COMPLEX) ) error stop 50
      end do
   end do

!! Test 7

   call initarr2d(aa,bb)

   ret = fnt7(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j,i+j,C_FLOAT_COMPLEX) ) error stop 52
         if ( bb(j,i) /= cmplx(i+j,i+j,C_DOUBLE_COMPLEX) ) error stop 54
      end do
   end do

!! Test 8

   call initarr2d(aa,bb)

   ret = fnt8(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j,i+j,C_FLOAT_COMPLEX) ) error stop 56
         if ( bb(j,i) /= cmplx(i+j,i+j,C_DOUBLE_COMPLEX) ) error stop 58
      end do
   end do

end program fxison08

subroutine initarr1d(x,y)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX) :: x(5)
   complex(C_DOUBLE_COMPLEX) :: y(5)

   do i = 1, 5
      x(i) = cmplx(i,i,C_FLOAT_COMPLEX)
   end do

   do i = 1, 5
      y(i) = cmplx(i,i,C_DOUBLE_COMPLEX)
   end do

end subroutine initarr1d

subroutine initarr2d(xx,yy)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX) :: xx(10,5)
   complex(C_DOUBLE_COMPLEX) :: yy(10,5)

   do i = 1, 5
      do j = 1, 10
         xx(j,i) = cmplx(i+j-1,i+j-1,C_FLOAT_COMPLEX)
      end do
   end do

   do i = 1, 5
      do j = 1, 10
         yy(j,i) = cmplx(i+j-1,i+j-1,C_DOUBLE_COMPLEX)
      end do
   end do

end subroutine initarr2d

