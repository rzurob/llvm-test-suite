!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisom08 cxisom08
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
!*  KEYWORD(S)                 : C_LONG_DOUBLE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_LONG_DOUBLE
!*      - using C functions with interface to FORTRAN functions
!*      - passing 1-dim and 2-dim array arguments
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisom08
   use ISO_C_BINDING

   interface
      real(C_LONG_DOUBLE) function fnt1(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE) :: a(5)
      end function fnt1
      real(C_LONG_DOUBLE) function fnt2(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), intent(in) :: a(5)
      end function fnt2
      real(C_LONG_DOUBLE) function fnt2a(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), intent(in) :: a(5)
      end function fnt2a
      real(C_LONG_DOUBLE) function fnt3(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), intent(inout) :: a(5)
      end function fnt3
      real(C_LONG_DOUBLE) function fnt4(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), intent(out) :: a(5)
      end function fnt4
      real(C_LONG_DOUBLE) function fnt5(aa)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE) :: aa(10,5)
      end function fnt5
      real(C_LONG_DOUBLE) function fnt6(aa)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), intent(in) :: aa(10,5)
      end function fnt6
      real(C_LONG_DOUBLE) function fnt6a(aa)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), intent(in) :: aa(10,5)
      end function fnt6a
      real(C_LONG_DOUBLE) function fnt7(aa)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), intent(inout) :: aa(10,5)
      end function fnt7
      real(C_LONG_DOUBLE) function fnt8(aa)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), intent(out) :: aa(10,5)
      end function fnt8
   end interface

   real(C_LONG_DOUBLE) :: a(5), aa(10,5), ret
   integer i, j

!! Test 1

   call initarr1d(a)

   ret = fnt1(a)

   do i = 1, 5
      if ( a(i) /= real(i+1,C_LONG_DOUBLE) ) error stop 20
   end do

!! Test 2

   call initarr1d(a)

   ret = fnt2(a)

   do i = 1, 5
      if ( a(i) /= real(i,C_LONG_DOUBLE) ) error stop 22
   end do

!! Test 2a

   call initarr1d(a)

   ret = fnt2a(a)

   do i = 1, 5
      if ( a(i) /= real(i,C_LONG_DOUBLE) ) error stop 24
   end do

!! Test 3

   call initarr1d(a)

   ret = fnt3(a)

   do i = 1, 5
      if ( a(i) /= real(i+1,C_LONG_DOUBLE) ) error stop 26
   end do

!! Test 4

   call initarr1d(a)

   ret = fnt4(a)

   do i = 1, 5
      if ( a(i) /= real(i+1,C_LONG_DOUBLE) ) error stop 28
   end do

!! Test 5

   call initarr2d(aa)

   ret = fnt5(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j,C_LONG_DOUBLE) ) error stop 30
      end do
   end do

!! Test 6

   call initarr2d(aa)

   ret = fnt6(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 32
      end do
   end do

!! Test 6a

   call initarr2d(aa)

   ret = fnt6a(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 34
      end do
   end do

!! Test 7

   call initarr2d(aa)

   ret = fnt7(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j,C_LONG_DOUBLE) ) error stop 36
      end do
   end do

!! Test 8

   call initarr2d(aa)

   ret = fnt8(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j,C_LONG_DOUBLE) ) error stop 38
      end do
   end do

end program fxisom08

subroutine initarr1d(x)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE) :: x(5)

   do i = 1, 5
      x(i) = real(i,C_LONG_DOUBLE)
   end do


end subroutine initarr1d

subroutine initarr2d(xx)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE) :: xx(10,5)

   do i = 1, 5
      do j = 1, 10
         xx(j,i) = real(i+j-1,C_LONG_DOUBLE)
      end do
   end do


end subroutine initarr2d

