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
!*  KEYWORD(S)                 : 16
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing 16
!*      - FORTRAN code only
!*      - passing 1-dim and 2-dim array arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisoq25
   use ISO_C_BINDING

   interface
      complex(16) function fnt1(a)
         use ISO_C_BINDING
         complex(16) :: a(5)
      end function fnt1
      complex(16) function fnt2(a)
         use ISO_C_BINDING
         complex(16), intent(in) :: a(5)
      end function fnt2
      complex(16) function fnt3(a)
         use ISO_C_BINDING
         complex(16), intent(inout) :: a(5)
      end function fnt3
      complex(16) function fnt4(a)
         use ISO_C_BINDING
         complex(16), intent(out) :: a(5)
      end function fnt4
      complex(16) function fnt5(aa)
         use ISO_C_BINDING
         complex(16) :: aa(10,5)
      end function fnt5
      complex(16) function fnt6(aa)
         use ISO_C_BINDING
         complex(16), intent(in) :: aa(10,5)
      end function fnt6
      complex(16) function fnt7(aa)
         use ISO_C_BINDING
         complex(16), intent(inout) :: aa(10,5)
      end function fnt7
      complex(16) function fnt8(aa)
         use ISO_C_BINDING
         complex(16), intent(out) :: aa(10,5)
      end function fnt8
   end interface

   complex(16) :: a(5), aa(10,5), ret
   integer i, j

!! Test 1

   call initarr1d(a)

   ret = fnt1(a)

   do i = 1, 5
      if ( a(i) /= cmplx(i+1,i+1,16) ) error stop 20
   end do

!! Test 2

   call initarr1d(a)

   ret = fnt2(a)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,16) ) error stop 22
   end do

!! Test 3

   call initarr1d(a)

   ret = fnt3(a)

   do i = 1, 5
      if ( a(i) /= cmplx(i+1,i+1,16) ) error stop 24
   end do

!! Test 4

   call initarr1d(a)

   ret = fnt4(a)

   do i = 1, 5
      if ( a(i) /= cmplx(i+1,i+1,16) ) error stop 26
   end do

!! Test 5

   call initarr2d(aa)

   ret = fnt5(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j,i+j,16) ) error stop 28
      end do
   end do

!! Test 6

   call initarr2d(aa)

   ret = fnt6(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,16) ) error stop 30
      end do
   end do

!! Test 7

   call initarr2d(aa)

   ret = fnt7(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j,i+j,16) ) error stop 32
      end do
   end do

!! Test 8

   call initarr2d(aa)

   ret = fnt8(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j,i+j,16) ) error stop 34
      end do
   end do

end program fxisoq25

subroutine initarr1d(x)
   use ISO_C_BINDING

   complex(16) :: x(5)

   do i = 1, 5
      x(i) = cmplx(i,i,16)
   end do


end subroutine initarr1d

subroutine initarr2d(xx)
   use ISO_C_BINDING

   complex(16) :: xx(10,5)

   do i = 1, 5
      do j = 1, 10
         xx(j,i) = cmplx(i+j-1,i+j-1,16)
      end do
   end do


end subroutine initarr2d

complex(16) function fnt1(a)
   use ISO_C_BINDING

   complex(16) :: a(5)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,16) ) error stop 36
      a(i) = cmplx(i+1,i+1,16)
   end do

   fnt1 = 0
end function fnt1

complex(16) function fnt2(a)
   use ISO_C_BINDING

   complex(16), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,16) ) error stop 38
   end do

   fnt2 = 0
end function fnt2

complex(16) function fnt3(a)
   use ISO_C_BINDING

   complex(16), intent(inout) :: a(5)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,16) ) error stop 40
      a(i) = cmplx(i+1,i+1,16)
   end do

   fnt3 = 0
end function fnt3

complex(16) function fnt4(a)
   use ISO_C_BINDING

   complex(16), intent(out) :: a(5)

   do i = 1, 5
      a(i) = cmplx(i+1,i+1,16)
   end do

   fnt4 = 0
end function fnt4

complex(16) function fnt5(aa)
   use ISO_C_BINDING

   complex(16) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,16) ) error stop 42
         aa(j,i) = cmplx(i+j,i+j,16)
      end do
   end do

   fnt5 = 0
end function fnt5

complex(16) function fnt6(aa)
   use ISO_C_BINDING

   complex(16), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,16) ) error stop 44
      end do
   end do

   fnt6 = 0
end function fnt6

complex(16) function fnt7(aa)
   use ISO_C_BINDING

   complex(16), intent(inout) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,16) ) error stop 46
         aa(j,i) = cmplx(i+j,i+j,16)
      end do
   end do

   fnt7 = 0
end function fnt7

complex(16) function fnt8(aa)
   use ISO_C_BINDING

   complex(16), intent(out) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         aa(j,i) = cmplx(i+j,i+j,16)
      end do
   end do

   fnt8 = 0
end function fnt8