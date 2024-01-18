!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisog08 cxisog08
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
!*      - using C functions with interface to FORTRAN functions
!*      - passing 1-dim and 2-dim array arguments
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisog08
   use ISO_C_BINDING

   interface
      integer(C_INT_FAST8_T) function fnt1(a,b)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T) :: a(5)
         integer(C_INTMAX_T) :: b(5)
      end function fnt1
      integer(C_INT_FAST8_T) function fnt2(a,b)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T), intent(in) :: a(5)
         integer(C_INTMAX_T), intent(in) :: b(5)
      end function fnt2
      integer(C_INT_FAST8_T) function fnt2a(a,b)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T), intent(in) :: a(5)
         integer(C_INTMAX_T), intent(in) :: b(5)
      end function fnt2a
      integer(C_INT_FAST8_T) function fnt3(a,b)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T), intent(inout) :: a(5)
         integer(C_INTMAX_T), intent(inout) :: b(5)
      end function fnt3
      integer(C_INT_FAST8_T) function fnt4(a,b)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T), intent(out) :: a(5)
         integer(C_INTMAX_T), intent(out) :: b(5)
      end function fnt4
      integer(C_INT_FAST8_T) function fnt5(aa,bb)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T) :: aa(10,5)
         integer(C_INTMAX_T) :: bb(10,5)
      end function fnt5
      integer(C_INT_FAST8_T) function fnt6(aa,bb)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T), intent(in) :: aa(10,5)
         integer(C_INTMAX_T), intent(in) :: bb(10,5)
      end function fnt6
      integer(C_INT_FAST8_T) function fnt6a(aa,bb)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T), intent(in) :: aa(10,5)
         integer(C_INTMAX_T), intent(in) :: bb(10,5)
      end function fnt6a
      integer(C_INT_FAST8_T) function fnt7(aa,bb)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T), intent(inout) :: aa(10,5)
         integer(C_INTMAX_T), intent(inout) :: bb(10,5)
      end function fnt7
      integer(C_INT_FAST8_T) function fnt8(aa,bb)
         use ISO_C_BINDING
         integer(C_INT_FAST8_T), intent(out) :: aa(10,5)
         integer(C_INTMAX_T), intent(out) :: bb(10,5)
      end function fnt8
   end interface

   integer(C_INT_FAST8_T) :: a(5), aa(10,5), ret
   integer(C_INTMAX_T) :: b(5), bb(10,5)
   integer i, j

!! Test 1

   call initarr1d(a,b)

   ret = fnt1(a,b)

   do i = 1, 5
      if ( a(i) /= i+1 ) error stop 20
      if ( b(i) /= i+1 ) error stop 22
   end do

!! Test 2

   call initarr1d(a,b)

   ret = fnt2(a,b)

   do i = 1, 5
      if ( a(i) /= i ) error stop 24
      if ( b(i) /= i ) error stop 26
   end do

!! Test 2a

   call initarr1d(a,b)

   ret = fnt2a(a,b)

   do i = 1, 5
      if ( a(i) /= i ) error stop 28
      if ( b(i) /= i ) error stop 30
   end do

!! Test 3

   call initarr1d(a,b)

   ret = fnt3(a,b)

   do i = 1, 5
      if ( a(i) /= i+1 ) error stop 32
      if ( b(i) /= i+1 ) error stop 34
   end do

!! Test 4

   call initarr1d(a,b)

   ret = fnt4(a,b)

   do i = 1, 5
      if ( a(i) /= i+1 ) error stop 36
      if ( b(i) /= i+1 ) error stop 38
   end do

!! Test 5

   call initarr2d(aa,bb)

   ret = fnt5(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j ) error stop 40
         if ( bb(j,i) /= i+j ) error stop 42
      end do
   end do

!! Test 6

   call initarr2d(aa,bb)

   ret = fnt6(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 44
         if ( bb(j,i) /= i+j-1 ) error stop 46
      end do
   end do

!! Test 6a

   call initarr2d(aa,bb)

   ret = fnt6a(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 48
         if ( bb(j,i) /= i+j-1 ) error stop 50
      end do
   end do

!! Test 7

   call initarr2d(aa,bb)

   ret = fnt7(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j ) error stop 52
         if ( bb(j,i) /= i+j ) error stop 54
      end do
   end do

!! Test 8

   call initarr2d(aa,bb)

   ret = fnt8(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j ) error stop 56
         if ( bb(j,i) /= i+j ) error stop 58
      end do
   end do

end program fxisog08

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

