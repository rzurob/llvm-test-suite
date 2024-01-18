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
!*  KEYWORD(S)                 : C_INT_LEAST8_T, C_INT_LEAST16_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_INT_LEAST8_T and C_INT_LEAST16_T
!*      - FORTRAN code only
!*      - passing 1-dim and 2-dim array arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisoe25
   use ISO_C_BINDING

   interface
      integer(C_INT_LEAST8_T) function fnt1(a,b)
         use ISO_C_BINDING
         integer(C_INT_LEAST8_T) :: a(5)
         integer(C_INT_LEAST16_T) :: b(5)
      end function fnt1
      integer(C_INT_LEAST8_T) function fnt2(a,b)
         use ISO_C_BINDING
         integer(C_INT_LEAST8_T), intent(in) :: a(5)
         integer(C_INT_LEAST16_T), intent(in) :: b(5)
      end function fnt2
      integer(C_INT_LEAST8_T) function fnt3(a,b)
         use ISO_C_BINDING
         integer(C_INT_LEAST8_T), intent(inout) :: a(5)
         integer(C_INT_LEAST16_T), intent(inout) :: b(5)
      end function fnt3
      integer(C_INT_LEAST8_T) function fnt4(a,b)
         use ISO_C_BINDING
         integer(C_INT_LEAST8_T), intent(out) :: a(5)
         integer(C_INT_LEAST16_T), intent(out) :: b(5)
      end function fnt4
      integer(C_INT_LEAST8_T) function fnt5(aa,bb)
         use ISO_C_BINDING
         integer(C_INT_LEAST8_T) :: aa(10,5)
         integer(C_INT_LEAST16_T) :: bb(10,5)
      end function fnt5
      integer(C_INT_LEAST8_T) function fnt6(aa,bb)
         use ISO_C_BINDING
         integer(C_INT_LEAST8_T), intent(in) :: aa(10,5)
         integer(C_INT_LEAST16_T), intent(in) :: bb(10,5)
      end function fnt6
      integer(C_INT_LEAST8_T) function fnt7(aa,bb)
         use ISO_C_BINDING
         integer(C_INT_LEAST8_T), intent(inout) :: aa(10,5)
         integer(C_INT_LEAST16_T), intent(inout) :: bb(10,5)
      end function fnt7
      integer(C_INT_LEAST8_T) function fnt8(aa,bb)
         use ISO_C_BINDING
         integer(C_INT_LEAST8_T), intent(out) :: aa(10,5)
         integer(C_INT_LEAST16_T), intent(out) :: bb(10,5)
      end function fnt8
   end interface

   integer(C_INT_LEAST8_T) :: a(5), aa(10,5), ret
   integer(C_INT_LEAST16_T) :: b(5), bb(10,5)
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

!! Test 3

   call initarr1d(a,b)

   ret = fnt3(a,b)

   do i = 1, 5
      if ( a(i) /= i+1 ) error stop 28
      if ( b(i) /= i+1 ) error stop 30
   end do

!! Test 4

   call initarr1d(a,b)

   ret = fnt4(a,b)

   do i = 1, 5
      if ( a(i) /= i+1 ) error stop 32
      if ( b(i) /= i+1 ) error stop 34
   end do

!! Test 5

   call initarr2d(aa,bb)

   ret = fnt5(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j ) error stop 36
         if ( bb(j,i) /= i+j ) error stop 38
      end do
   end do

!! Test 6

   call initarr2d(aa,bb)

   ret = fnt6(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 40
         if ( bb(j,i) /= i+j-1 ) error stop 42
      end do
   end do

!! Test 7

   call initarr2d(aa,bb)

   ret = fnt7(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j ) error stop 44
         if ( bb(j,i) /= i+j ) error stop 46
      end do
   end do

!! Test 8

   call initarr2d(aa,bb)

   ret = fnt8(aa,bb)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j ) error stop 48
         if ( bb(j,i) /= i+j ) error stop 50
      end do
   end do

end program fxisoe25

subroutine initarr1d(x,y)
   use ISO_C_BINDING

   integer(C_INT_LEAST8_T) :: x(5)
   integer(C_INT_LEAST16_T) :: y(5)

   do i = 1, 5
      x(i) = i
   end do

   do i = 1, 5
      y(i) = i
   end do

end subroutine initarr1d

subroutine initarr2d(xx,yy)
   use ISO_C_BINDING

   integer(C_INT_LEAST8_T) :: xx(10,5)
   integer(C_INT_LEAST16_T) :: yy(10,5)

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

integer(C_INT_LEAST8_T) function fnt1(a,b)
   use ISO_C_BINDING

   integer(C_INT_LEAST8_T) :: a(5)
   integer(C_INT_LEAST16_T) :: b(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 52
      a(i) = i+1
      if ( b(i) /= i ) error stop 54
      b(i) = i+1
   end do

   fnt1 = 0
end function fnt1

integer(C_INT_LEAST8_T) function fnt2(a,b)
   use ISO_C_BINDING

   integer(C_INT_LEAST8_T), intent(in) :: a(5)
   integer(C_INT_LEAST16_T), intent(in) :: b(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 56
      if ( b(i) /= i ) error stop 58
   end do

   fnt2 = 0
end function fnt2

integer(C_INT_LEAST8_T) function fnt3(a,b)
   use ISO_C_BINDING

   integer(C_INT_LEAST8_T), intent(inout) :: a(5)
   integer(C_INT_LEAST16_T), intent(inout) :: b(5)

   do i = 1, 5
      if ( a(i) /= i ) error stop 60
      a(i) = i+1
      if ( b(i) /= i ) error stop 62
      b(i) = i+1
   end do

   fnt3 = 0
end function fnt3

integer(C_INT_LEAST8_T) function fnt4(a,b)
   use ISO_C_BINDING

   integer(C_INT_LEAST8_T), intent(out) :: a(5)
   integer(C_INT_LEAST16_T), intent(out) :: b(5)

   do i = 1, 5
      a(i) = i+1
      b(i) = i+1
   end do

   fnt4 = 0
end function fnt4

integer(C_INT_LEAST8_T) function fnt5(aa,bb)
   use ISO_C_BINDING

   integer(C_INT_LEAST8_T) :: aa(10,5)
   integer(C_INT_LEAST16_T) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 64
         aa(j,i) = i+j
         if ( bb(j,i) /= i+j-1 ) error stop 66
         bb(j,i) = i+j
      end do
   end do

   fnt5 = 0
end function fnt5

integer(C_INT_LEAST8_T) function fnt6(aa,bb)
   use ISO_C_BINDING

   integer(C_INT_LEAST8_T), intent(in) :: aa(10,5)
   integer(C_INT_LEAST16_T), intent(in) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 68
         if ( bb(j,i) /= i+j-1 ) error stop 70
      end do
   end do

   fnt6 = 0
end function fnt6

integer(C_INT_LEAST8_T) function fnt7(aa,bb)
   use ISO_C_BINDING

   integer(C_INT_LEAST8_T), intent(inout) :: aa(10,5)
   integer(C_INT_LEAST16_T), intent(inout) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= i+j-1 ) error stop 72
         aa(j,i) = i+j
         if ( bb(j,i) /= i+j-1 ) error stop 74
         bb(j,i) = i+j
      end do
   end do

   fnt7 = 0
end function fnt7

integer(C_INT_LEAST8_T) function fnt8(aa,bb)
   use ISO_C_BINDING

   integer(C_INT_LEAST8_T), intent(out) :: aa(10,5)
   integer(C_INT_LEAST16_T), intent(out) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         aa(j,i) = i+j
         bb(j,i) = i+j
      end do
   end do

   fnt8 = 0
end function fnt8
