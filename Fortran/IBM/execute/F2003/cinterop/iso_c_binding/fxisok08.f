!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 4/23/2002
!*
!*  PRIMARY FUNCTIONS TESTED   : ISO_C_BINDING module
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : C_CHAR, C_SIGNED_CHAR
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_CHAR and C_SIGNED_CHAR
!*      - using C functions with interface to FORTRAN functions
!*      - passing 1-dim and 2-dim array arguments
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisok08
   use ISO_C_BINDING

   interface
      integer(C_SIGNED_CHAR) function fnt1(a,b)
         use ISO_C_BINDING
         character(C_CHAR) :: a(4)
         integer(C_SIGNED_CHAR) :: b(4)
      end function fnt1
      integer(C_SIGNED_CHAR) function fnt2(a,b)
         use ISO_C_BINDING
         character(C_CHAR), intent(in) :: a(4)
         integer(C_SIGNED_CHAR), intent(in) :: b(4)
      end function fnt2
      integer(C_SIGNED_CHAR) function fnt2a(a,b)
         use ISO_C_BINDING
         character(C_CHAR), intent(in) :: a(4)
         integer(C_SIGNED_CHAR), intent(in) :: b(4)
      end function fnt2a
      integer(C_SIGNED_CHAR) function fnt3(a,b)
         use ISO_C_BINDING
         character(C_CHAR), intent(inout) :: a(4)
         integer(C_SIGNED_CHAR), intent(inout) :: b(4)
      end function fnt3
      integer(C_SIGNED_CHAR) function fnt4(a,b)
         use ISO_C_BINDING
         character(C_CHAR), intent(out) :: a(4)
         integer(C_SIGNED_CHAR), intent(out) :: b(4)
      end function fnt4
      integer(C_SIGNED_CHAR) function fnt5(aa,bb)
         use ISO_C_BINDING
         character(C_CHAR) :: aa(6,4)
         integer(C_SIGNED_CHAR) :: bb(6,4)
      end function fnt5
      integer(C_SIGNED_CHAR) function fnt6(aa,bb)
         use ISO_C_BINDING
         character(C_CHAR), intent(in) :: aa(6,4)
         integer(C_SIGNED_CHAR), intent(in) :: bb(6,4)
      end function fnt6
      integer(C_SIGNED_CHAR) function fnt6a(aa,bb)
         use ISO_C_BINDING
         character(C_CHAR), intent(in) :: aa(6,4)
         integer(C_SIGNED_CHAR), intent(in) :: bb(6,4)
      end function fnt6a
      integer(C_SIGNED_CHAR) function fnt7(aa,bb)
         use ISO_C_BINDING
         character(C_CHAR), intent(inout) :: aa(6,4)
         integer(C_SIGNED_CHAR), intent(inout) :: bb(6,4)
      end function fnt7
      integer(C_SIGNED_CHAR) function fnt8(aa,bb)
         use ISO_C_BINDING
         character(C_CHAR), intent(out) :: aa(6,4)
         integer(C_SIGNED_CHAR), intent(out) :: bb(6,4)
      end function fnt8
   end interface

   character(C_CHAR) :: a(4), aa(6,4)
   integer(C_SIGNED_CHAR) :: b(4), bb(6,4)
   integer :: ret
   integer i, j

!! Test 1

   call initarr1d(a,b)

   ret = fnt1(a,b)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i+3) ) error stop 20
      if ( b(i) /= iachar('A')+i+3 ) error stop 22
   end do

!! Test 2

   call initarr1d(a,b)

   ret = fnt2(a,b)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i-1) ) error stop 24
      if ( b(i) /= iachar('A')+i-1 ) error stop 26
   end do

!! Test 2a

   call initarr1d(a,b)

   ret = fnt2a(a,b)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i-1) ) error stop 28
      if ( b(i) /= iachar('A')+i-1 ) error stop 30
   end do

!! Test 3

   call initarr1d(a,b)

   ret = fnt3(a,b)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i+3) ) error stop 32
      if ( b(i) /= iachar('A')+i+3 ) error stop 34
   end do

!! Test 4

   call initarr1d(a,b)

   ret = fnt4(a,b)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i+3) ) error stop 36
      if ( b(i) /= iachar('A')+i+3 ) error stop 38
   end do

!! Test 5

   call initarr2d(aa,bb)

   ret = fnt5(aa,bb)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+j) ) error stop 40
         if ( bb(j,i) /= iachar('A')+(i-1)*6+j ) error stop 42
      end do
   end do

!! Test 6

   call initarr2d(aa,bb)

   ret = fnt6(aa,bb)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+(j-1)) ) error stop 44
         if ( bb(j,i) /= iachar('A')+(i-1)*6+(j-1) ) error stop 46
      end do
   end do

!! Test 6a

   call initarr2d(aa,bb)

   ret = fnt6a(aa,bb)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+(j-1)) ) error stop 48
         if ( bb(j,i) /= iachar('A')+(i-1)*6+(j-1) ) error stop 50
      end do
   end do

!! Test 7

   call initarr2d(aa,bb)

   ret = fnt7(aa,bb)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+j) ) error stop 52
         if ( bb(j,i) /= iachar('A')+(i-1)*6+j ) error stop 54
      end do
   end do

!! Test 8

   call initarr2d(aa,bb)

   ret = fnt8(aa,bb)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+j) ) error stop 56
         if ( bb(j,i) /= iachar('A')+(i-1)*6+j ) error stop 58
      end do
   end do

end program fxisok08

subroutine initarr1d(x,y)
   use ISO_C_BINDING

   character(C_CHAR) :: x(4)
   integer(C_SIGNED_CHAR) :: y(4)

   do i = 1, 4
      x(i) = achar(iachar('A')+i-1)
   end do

   do i = 1, 4
      y(i) = iachar('A')+i-1
   end do

end subroutine initarr1d

subroutine initarr2d(xx,yy)
   use ISO_C_BINDING

   character(C_CHAR) :: xx(6,4)
   integer(C_SIGNED_CHAR) :: yy(6,4)

   do i = 1, 4
      do j = 1, 6
         xx(j,i) = achar(iachar('A')+(i-1)*6+(j-1))
      end do
   end do

   do i = 1, 4
      do j = 1, 6
         yy(j,i) = iachar('A')+(i-1)*6+(j-1)
      end do
   end do

end subroutine initarr2d

