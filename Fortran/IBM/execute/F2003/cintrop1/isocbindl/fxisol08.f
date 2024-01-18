!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisol08 cxisol08
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
!*  KEYWORD(S)                 : C_FLOAT, C_DOUBLE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_FLOAT and C_DOUBLE
!*      - using C functions with interface to FORTRAN functions
!*      - passing 1-dim and 2-dim array arguments
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisol08
   use ISO_C_BINDING

   interface
      real(C_FLOAT) function fnt1(a,b)
         use ISO_C_BINDING
         real(C_FLOAT) :: a(5)
         real(C_DOUBLE) :: b(5)
      end function fnt1
      real(C_FLOAT) function fnt2(a,b)
         use ISO_C_BINDING
         real(C_FLOAT), intent(in) :: a(5)
         real(C_DOUBLE), intent(in) :: b(5)
      end function fnt2
      real(C_FLOAT) function fnt2a(a,b)
         use ISO_C_BINDING
         real(C_FLOAT), intent(in) :: a(5)
         real(C_DOUBLE), intent(in) :: b(5)
      end function fnt2a
      real(C_FLOAT) function fnt3(a,b)
         use ISO_C_BINDING
         real(C_FLOAT), intent(inout) :: a(5)
         real(C_DOUBLE), intent(inout) :: b(5)
      end function fnt3
      real(C_FLOAT) function fnt4(a,b)
         use ISO_C_BINDING
         real(C_FLOAT), intent(out) :: a(5)
         real(C_DOUBLE), intent(out) :: b(5)
      end function fnt4
      real(C_FLOAT) function fnt5(aa,bb)
         use ISO_C_BINDING
         real(C_FLOAT) :: aa(10,5)
         real(C_DOUBLE) :: bb(10,5)
      end function fnt5
      real(C_FLOAT) function fnt6(aa,bb)
         use ISO_C_BINDING
         real(C_FLOAT), intent(in) :: aa(10,5)
         real(C_DOUBLE), intent(in) :: bb(10,5)
      end function fnt6
      real(C_FLOAT) function fnt6a(aa,bb)
         use ISO_C_BINDING
         real(C_FLOAT), intent(in) :: aa(10,5)
         real(C_DOUBLE), intent(in) :: bb(10,5)
      end function fnt6a
      real(C_FLOAT) function fnt7(aa,bb)
         use ISO_C_BINDING
         real(C_FLOAT), intent(inout) :: aa(10,5)
         real(C_DOUBLE), intent(inout) :: bb(10,5)
      end function fnt7
      real(C_FLOAT) function fnt8(aa,bb)
         use ISO_C_BINDING
         real(C_FLOAT), intent(out) :: aa(10,5)
         real(C_DOUBLE), intent(out) :: bb(10,5)
      end function fnt8
   end interface

   real(C_FLOAT) :: a(5), aa(10,5), ret
   real(C_DOUBLE) :: b(5), bb(10,5)
   integer i, j

!! Test 1

   call initarr1d(a,b)

   ret = fnt1(a,b)

   do i = 1, 5
      if ( a(i) /= real(i+1,C_FLOAT) ) error stop 20
      if ( b(i) /= real(i+1,C_DOUBLE) ) error stop 22
   end do

!! Test 2

   call initarr1d(a,b)

   ret = fnt2(a,b)

   do i = 1, 5
      if ( a(i) /= real(i,C_FLOAT) ) error stop 24
      if ( b(i) /= real(i,C_DOUBLE) ) error stop 26
   end do

!! Test 2a

   call initarr1d(a,b)

   ret = fnt2a(a,b)

   do i = 1, 5
      if ( a(i) /= real(i,C_FLOAT) ) error stop 28
      if ( b(i) /= real(i,C_DOUBLE) ) error stop 30
   end do

!! Test 3

   call initarr1d(a,b)

   ret = fnt3(a,b)

   do i = 1, 5
      if ( a(i) /= real(i+1,C_FLOAT) ) error stop 32
      if ( b(i) /= real(i+1,C_DOUBLE) ) error stop 34
   end do

!! Test 4

   call initarr1d(a,b)

   ret = fnt4(a,b)

   do i = 1, 5
      if ( a(i) /= real(i+1,C_FLOAT) ) error stop 36
      if ( b(i) /= real(i+1,C_DOUBLE) ) error stop 38
   end do

!! Test 5

   call initarr2d(aa,bb)

   ret = fnt5(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j,C_FLOAT) ) error stop 40
         if ( bb(j,i) /= real(i+j,C_DOUBLE) ) error stop 42
      end do
   end do

!! Test 6

   call initarr2d(aa,bb)

   ret = fnt6(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_FLOAT) ) error stop 44
         if ( bb(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 46
      end do
   end do

!! Test 6a

   call initarr2d(aa,bb)

   ret = fnt6a(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_FLOAT) ) error stop 48
         if ( bb(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 50
      end do
   end do

!! Test 7

   call initarr2d(aa,bb)

   ret = fnt7(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j,C_FLOAT) ) error stop 52
         if ( bb(j,i) /= real(i+j,C_DOUBLE) ) error stop 54
      end do
   end do

!! Test 8

   call initarr2d(aa,bb)

   ret = fnt8(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j,C_FLOAT) ) error stop 56
         if ( bb(j,i) /= real(i+j,C_DOUBLE) ) error stop 58
      end do
   end do

end program fxisol08

subroutine initarr1d(x,y)
   use ISO_C_BINDING

   real(C_FLOAT) :: x(5)
   real(C_DOUBLE) :: y(5)

   do i = 1, 5
      x(i) = real(i,C_FLOAT)
   end do

   do i = 1, 5
      y(i) = real(i,C_DOUBLE)
   end do

end subroutine initarr1d

subroutine initarr2d(xx,yy)
   use ISO_C_BINDING

   real(C_FLOAT) :: xx(10,5)
   real(C_DOUBLE) :: yy(10,5)

   do i = 1, 5
      do j = 1, 10
         xx(j,i) = real(i+j-1,C_FLOAT)
      end do
   end do

   do i = 1, 5
      do j = 1, 10
         yy(j,i) = real(i+j-1,C_DOUBLE)
      end do
   end do

end subroutine initarr2d

