!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa01.presh fxisoi25
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
!*  KEYWORD(S)                 : C_INT_FAST16_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_INT_FAST16_T
!*      - FORTRAN code only
!*      - passing 1-dim and 2-dim array arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisoi25
   use ISO_C_BINDING

   interface
      integer(C_INT_FAST16_T) function fnt1(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T) :: a(5)
      end function fnt1
      integer(C_INT_FAST16_T) function fnt2(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), intent(in) :: a(5)
      end function fnt2
      integer(C_INT_FAST16_T) function fnt3(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), intent(inout) :: a(5)
      end function fnt3
      integer(C_INT_FAST16_T) function fnt4(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), intent(out) :: a(5)
      end function fnt4
      integer(C_INT_FAST16_T) function fnt5(aa)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T) :: aa(10,5)
      end function fnt5
      integer(C_INT_FAST16_T) function fnt6(aa)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), intent(in) :: aa(10,5)
      end function fnt6
      integer(C_INT_FAST16_T) function fnt7(aa)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), intent(inout) :: aa(10,5)
      end function fnt7
      integer(C_INT_FAST16_T) function fnt8(aa)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), intent(out) :: aa(10,5)
      end function fnt8
   end interface

   integer(C_INT_FAST16_T) :: a(5), aa(10,5), ret
   integer i, j

!! Test 1

   call initarr1d(a)

   ret = fnt1(a)

   do i = 1, 5
      if ( a(i) /= i+1 ) error stop 20
   end do

!! Test 2

   call initarr1d(a)

   ret = fnt2(a)

   do i = 1, 5
      if ( a(i) /= i ) error stop 22
   end do

!! Test 3

   call initarr1d(a)

   ret = fnt3(a)

   do i = 1, 5
      if ( a(i) /= i+1 ) error stop 24
   end do

!! Test 4

   call initarr1d(a)

   ret = fnt4(a)

   do i = 1, 5
      if ( a(i) /= i+1 ) error stop 26
   end do

!! Test 5

   call initarr2d(aa)

   ret = fnt5(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j ) error stop 28
      end do
   end do

!! Test 6

   call initarr2d(aa)

   ret = fnt6(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 30
      end do
   end do

!! Test 7

   call initarr2d(aa)

   ret = fnt7(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j ) error stop 32
      end do
   end do

!! Test 8

   call initarr2d(aa)

   ret = fnt8(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j ) error stop 34
      end do
   end do

end program fxisoi25

subroutine initarr1d(x)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T) :: x(5)

   do i = 1, 5
      x(i) = i
   end do


end subroutine initarr1d

subroutine initarr2d(xx)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T) :: xx(10,5)

   do i = 1, 5
      do j = 1, 10
         xx(j,i) = i+j-1
      end do
   end do


end subroutine initarr2d

integer(C_INT_FAST16_T) function fnt1(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T) :: a(5)
   
   do i = 1, 5
      if ( a(i) /= i ) error stop 36
      a(i) = i+1
   end do

   fnt1 = 0
end function fnt1

integer(C_INT_FAST16_T) function fnt2(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 38
   end do

   fnt2 = 0
end function fnt2

integer(C_INT_FAST16_T) function fnt3(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(inout) :: a(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 40
      a(i) = i+1
   end do

   fnt3 = 0
end function fnt3

integer(C_INT_FAST16_T) function fnt4(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(out) :: a(5)

   do i = 1, 5
      a(i) = i+1
   end do

   fnt4 = 0
end function fnt4

integer(C_INT_FAST16_T) function fnt5(aa)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T) :: aa(10,5)
   
   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 42
         aa(j,i) = i+j
      end do
   end do

   fnt5 = 0
end function fnt5

integer(C_INT_FAST16_T) function fnt6(aa)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 44
      end do
   end do

   fnt6 = 0
end function fnt6

integer(C_INT_FAST16_T) function fnt7(aa)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(inout) :: aa(10,5)
   
   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 46
         aa(j,i) = i+j
      end do
   end do

   fnt7 = 0
end function fnt7

integer(C_INT_FAST16_T) function fnt8(aa)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(out) :: aa(10,5)
   
   do i = 1, 5
      do j = 1, 10
         aa(j,i) = i+j
      end do
   end do

   fnt8 = 0
end function fnt8
