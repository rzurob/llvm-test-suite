!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisog10 cxisog10
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
!*  KEYWORD(S)                 : C_INT_FAST8_T, C_INTMAX_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_INT_FAST8_T and C_INTMAX_T
!*      - using C functions with interface to FORTRAN subroutines
!*      - passing 1-dim and 2-dim array arguments
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisog10
   use ISO_C_BINDING

   interface
      subroutine sub1(a,b)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T) :: a(5)
         integer(C_INTMAX_T) :: b(5)
      end subroutine sub1
      subroutine sub2(a,b)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T), intent(in) :: a(5)
         integer(C_INTMAX_T), intent(in) :: b(5)
      end subroutine sub2
      subroutine sub2a(a,b)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T), intent(in) :: a(5)
         integer(C_INTMAX_T), intent(in) :: b(5)
      end subroutine sub2a
      subroutine sub3(a,b)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T), intent(inout) :: a(5)
         integer(C_INTMAX_T), intent(inout) :: b(5)
      end subroutine sub3
      subroutine sub4(a,b)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T), intent(out) :: a(5)
         integer(C_INTMAX_T), intent(out) :: b(5)
      end subroutine sub4
      subroutine sub5(aa,bb)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T) :: aa(10,5)
         integer(C_INTMAX_T) :: bb(10,5)
      end subroutine sub5
      subroutine sub6(aa,bb)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T), intent(in) :: aa(10,5)
         integer(C_INTMAX_T), intent(in) :: bb(10,5)
      end subroutine sub6
      subroutine sub6a(aa,bb)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T), intent(in) :: aa(10,5)
         integer(C_INTMAX_T), intent(in) :: bb(10,5)
      end subroutine sub6a
      subroutine sub7(aa,bb)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T), intent(inout) :: aa(10,5)
         integer(C_INTMAX_T), intent(inout) :: bb(10,5)
      end subroutine sub7
      subroutine sub8(aa,bb)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T), intent(out) :: aa(10,5)
         integer(C_INTMAX_T), intent(out) :: bb(10,5)
      end subroutine sub8
   end interface

   integer(C_INT_FAST8_T) :: a(5), aa(10,5), ret
   integer(C_INTMAX_T) :: b(5), bb(10,5)
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

end program fxisog10

subroutine initarr1d(x,y)
   use ISO_C_BINDING

   integer(C_INT_FAST8_T) :: x(5)
   integer(C_INTMAX_T) :: y(5)

   do i = 1, 5
      x(i) = i
   end do

   do i = 1, 5
      y(i) = i
   end do

end subroutine initarr1d

subroutine initarr2d(xx,yy)
   use ISO_C_BINDING

   integer(C_INT_FAST8_T) :: xx(10,5)
   integer(C_INTMAX_T) :: yy(10,5)

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

