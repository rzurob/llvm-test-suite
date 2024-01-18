!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisob11 cxisob10
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
!*  KEYWORD(S)                 : C_LONG, C_LONG_LONG
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_LONG and C_LONG_LONG
!*      - using C functions with interface to FORTRAN subroutines
!*      - subroutine interfaces defined in a module
!*      - passing 1-dim and 2-dim array arguments
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob11
   interface
      subroutine sub1(a,b)
         use ISO_C_BINDING
         integer(C_LONG) :: a(5)
         integer(C_LONG_LONG) :: b(5)
      end subroutine sub1
      subroutine sub2(a,b)
         use ISO_C_BINDING
         integer(C_LONG), intent(in) :: a(5)
         integer(C_LONG_LONG), intent(in) :: b(5)
      end subroutine sub2
      subroutine sub2a(a,b)
         use ISO_C_BINDING
         integer(C_LONG), intent(in) :: a(5)
         integer(C_LONG_LONG), intent(in) :: b(5)
      end subroutine sub2a
      subroutine sub3(a,b)
         use ISO_C_BINDING
         integer(C_LONG), intent(inout) :: a(5)
         integer(C_LONG_LONG), intent(inout) :: b(5)
      end subroutine sub3
      subroutine sub4(a,b)
         use ISO_C_BINDING
         integer(C_LONG), intent(out) :: a(5)
         integer(C_LONG_LONG), intent(out) :: b(5)
      end subroutine sub4
      subroutine sub5(aa,bb)
         use ISO_C_BINDING
         integer(C_LONG) :: aa(10,5)
         integer(C_LONG_LONG) :: bb(10,5)
      end subroutine sub5
      subroutine sub6(aa,bb)
         use ISO_C_BINDING
         integer(C_LONG), intent(in) :: aa(10,5)
         integer(C_LONG_LONG), intent(in) :: bb(10,5)
      end subroutine sub6
      subroutine sub6a(aa,bb)
         use ISO_C_BINDING
         integer(C_LONG), intent(in) :: aa(10,5)
         integer(C_LONG_LONG), intent(in) :: bb(10,5)
      end subroutine sub6a
      subroutine sub7(aa,bb)
         use ISO_C_BINDING
         integer(C_LONG), intent(inout) :: aa(10,5)
         integer(C_LONG_LONG), intent(inout) :: bb(10,5)
      end subroutine sub7
      subroutine sub8(aa,bb)
         use ISO_C_BINDING
         integer(C_LONG), intent(out) :: aa(10,5)
         integer(C_LONG_LONG), intent(out) :: bb(10,5)
      end subroutine sub8
   end interface
end module mxisob11

program fxisob11
   use ISO_C_BINDING
   use mxisob11

   integer(C_LONG) :: a(5), aa(10,5), ret
   integer(C_LONG_LONG) :: b(5), bb(10,5)
   integer i, j

!! Test 1

   call initarr1d(a,b)

   call sub1(a,b)

   do i = 1, 5
      if ( a(i) /= i+1 ) error stop 20
      if ( b(i) /= i+1 ) error stop 22
   end do

!! Test 2

   call initarr1d(a,b)

   call sub2(a,b)

   do i = 1, 5
      if ( a(i) /= i ) error stop 24
      if ( b(i) /= i ) error stop 26
   end do

!! Test 2a

   call initarr1d(a,b)

   call sub2a(a,b)

   do i = 1, 5
      if ( a(i) /= i ) error stop 28
      if ( b(i) /= i ) error stop 30
   end do

!! Test 3

   call initarr1d(a,b)

   call sub3(a,b)

   do i = 1, 5
      if ( a(i) /= i+1 ) error stop 32
      if ( b(i) /= i+1 ) error stop 34
   end do

!! Test 4

   call initarr1d(a,b)

   call sub4(a,b)

   do i = 1, 5
      if ( a(i) /= i+1 ) error stop 36
      if ( b(i) /= i+1 ) error stop 38
   end do

!! Test 5

   call initarr2d(aa,bb)

   call sub5(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j ) error stop 40
         if ( bb(j,i) /= i+j ) error stop 42
      end do
   end do

!! Test 6

   call initarr2d(aa,bb)

   call sub6(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 44
         if ( bb(j,i) /= i+j-1 ) error stop 46
      end do
   end do

!! Test 6a

   call initarr2d(aa,bb)

   call sub6a(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 48
         if ( bb(j,i) /= i+j-1 ) error stop 50
      end do
   end do

!! Test 7

   call initarr2d(aa,bb)

   call sub7(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j ) error stop 52
         if ( bb(j,i) /= i+j ) error stop 54
      end do
   end do

!! Test 8

   call initarr2d(aa,bb)

   call sub8(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j ) error stop 56
         if ( bb(j,i) /= i+j ) error stop 58
      end do
   end do

end program fxisob11

subroutine initarr1d(x,y)
   use ISO_C_BINDING

   integer(C_LONG) :: x(5)
   integer(C_LONG_LONG) :: y(5)

   do i = 1, 5
      x(i) = i
   end do

   do i = 1, 5
      y(i) = i
   end do

end subroutine initarr1d

subroutine initarr2d(xx,yy)
   use ISO_C_BINDING

   integer(C_LONG) :: xx(10,5)
   integer(C_LONG_LONG) :: yy(10,5)

   do i = 1, 5
      do j = 1, 10
         xx(j,i) = i+j-1
      end do
   end do

   do i = 1, 5
      do j = 1, 10
         yy(j,i) = i+j-1
      end do
   end do

end subroutine initarr2d

