!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa01.presh fxisol25
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
!*      - FORTRAN code only
!*      - passing 1-dim and 2-dim array arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisol25
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

!! Test 3

   call initarr1d(a,b)

   ret = fnt3(a,b)

   do i = 1, 5
      if ( a(i) /= real(i+1,C_FLOAT) ) error stop 28
      if ( b(i) /= real(i+1,C_DOUBLE) ) error stop 30
   end do

!! Test 4

   call initarr1d(a,b)

   ret = fnt4(a,b)

   do i = 1, 5
      if ( a(i) /= real(i+1,C_FLOAT) ) error stop 32
      if ( b(i) /= real(i+1,C_DOUBLE) ) error stop 34
   end do

!! Test 5

   call initarr2d(aa,bb)

   ret = fnt5(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j,C_FLOAT) ) error stop 36
         if ( bb(j,i) /= real(i+j,C_DOUBLE) ) error stop 38
      end do
   end do

!! Test 6

   call initarr2d(aa,bb)

   ret = fnt6(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_FLOAT) ) error stop 40
         if ( bb(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 42
      end do
   end do

!! Test 7

   call initarr2d(aa,bb)

   ret = fnt7(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j,C_FLOAT) ) error stop 44
         if ( bb(j,i) /= real(i+j,C_DOUBLE) ) error stop 46
      end do
   end do

!! Test 8

   call initarr2d(aa,bb)

   ret = fnt8(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j,C_FLOAT) ) error stop 48
         if ( bb(j,i) /= real(i+j,C_DOUBLE) ) error stop 50
      end do
   end do

end program fxisol25

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

real(C_FLOAT) function fnt1(a,b)
   use ISO_C_BINDING

   real(C_FLOAT) :: a(5)
   real(C_DOUBLE) :: b(5)
   
   do i = 1, 5
      if ( a(i) /= real(i,C_FLOAT) ) error stop 52
      a(i) = real(i+1,C_FLOAT)
      if ( b(i) /= real(i,C_DOUBLE) ) error stop 54
      b(i) = real(i+1,C_DOUBLE)
   end do

   fnt1 = 0
end function fnt1

real(C_FLOAT) function fnt2(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), intent(in) :: a(5)
   real(C_DOUBLE), intent(in) :: b(5)

   do i = 1, 5
      if ( a(i) /= real(i,C_FLOAT) ) error stop 56
      if ( b(i) /= real(i,C_DOUBLE) ) error stop 58
   end do

   fnt2 = 0
end function fnt2

real(C_FLOAT) function fnt3(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), intent(inout) :: a(5)
   real(C_DOUBLE), intent(inout) :: b(5)

   do i = 1, 5
      if ( a(i) /= real(i,C_FLOAT) ) error stop 60
      a(i) = real(i+1,C_FLOAT)
      if ( b(i) /= real(i,C_DOUBLE) ) error stop 62
      b(i) = real(i+1,C_DOUBLE)
   end do

   fnt3 = 0
end function fnt3

real(C_FLOAT) function fnt4(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), intent(out) :: a(5)
   real(C_DOUBLE), intent(out) :: b(5)

   do i = 1, 5
      a(i) = real(i+1,C_FLOAT)
      b(i) = real(i+1,C_DOUBLE)
   end do

   fnt4 = 0
end function fnt4

real(C_FLOAT) function fnt5(aa,bb)
   use ISO_C_BINDING

   real(C_FLOAT) :: aa(10,5)
   real(C_DOUBLE) :: bb(10,5)
   
   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_FLOAT) ) error stop 64
         aa(j,i) = real(i+j,C_FLOAT)
         if ( bb(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 66
         bb(j,i) = real(i+j,C_DOUBLE)
      end do
   end do

   fnt5 = 0
end function fnt5

real(C_FLOAT) function fnt6(aa,bb)
   use ISO_C_BINDING

   real(C_FLOAT), intent(in) :: aa(10,5)
   real(C_DOUBLE), intent(in) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_FLOAT) ) error stop 68
         if ( bb(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 70
      end do
   end do

   fnt6 = 0
end function fnt6

real(C_FLOAT) function fnt7(aa,bb)
   use ISO_C_BINDING

   real(C_FLOAT), intent(inout) :: aa(10,5)
   real(C_DOUBLE), intent(inout) :: bb(10,5)
   
   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_FLOAT) ) error stop 72
         aa(j,i) = real(i+j,C_FLOAT)
         if ( bb(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 74
         bb(j,i) = real(i+j,C_DOUBLE)
      end do
   end do

   fnt7 = 0
end function fnt7

real(C_FLOAT) function fnt8(aa,bb)
   use ISO_C_BINDING

   real(C_FLOAT), intent(out) :: aa(10,5)
   real(C_DOUBLE), intent(out) :: bb(10,5)
   
   do i = 1, 5
      do j = 1, 10
         aa(j,i) = real(i+j,C_FLOAT)
         bb(j,i) = real(i+j,C_DOUBLE)
      end do
   end do

   fnt8 = 0
end function fnt8
