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
!*  KEYWORD(S)                 : C_LONG_DOUBLE_COMPLEX
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_LONG_DOUBLE_COMPLEX
!*      - using C functions with interface to FORTRAN functions
!*      - function interfaces defined in a module
!*      - passing 1-dim and 2-dim array arguments
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob09
   interface
      complex(C_LONG_DOUBLE_COMPLEX) function fnt1(a)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX) :: a(5)
      end function fnt1
      complex(C_LONG_DOUBLE_COMPLEX) function fnt2(a)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX), intent(in) :: a(5)
      end function fnt2
      complex(C_LONG_DOUBLE_COMPLEX) function fnt2a(a)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX), intent(in) :: a(5)
      end function fnt2a
      complex(C_LONG_DOUBLE_COMPLEX) function fnt3(a)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX), intent(inout) :: a(5)
      end function fnt3
      complex(C_LONG_DOUBLE_COMPLEX) function fnt4(a)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX), intent(out) :: a(5)
      end function fnt4
      complex(C_LONG_DOUBLE_COMPLEX) function fnt5(aa)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX) :: aa(10,5)
      end function fnt5
      complex(C_LONG_DOUBLE_COMPLEX) function fnt6(aa)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX), intent(in) :: aa(10,5)
      end function fnt6
      complex(C_LONG_DOUBLE_COMPLEX) function fnt6a(aa)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX), intent(in) :: aa(10,5)
      end function fnt6a
      complex(C_LONG_DOUBLE_COMPLEX) function fnt7(aa)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX), intent(inout) :: aa(10,5)
      end function fnt7
      complex(C_LONG_DOUBLE_COMPLEX) function fnt8(aa)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX), intent(out) :: aa(10,5)
      end function fnt8
   end interface
end module mxisob09

program fxisoo09
   use ISO_C_BINDING
   use mxisob09

   complex(C_LONG_DOUBLE_COMPLEX) :: a(5), aa(10,5), ret
   integer i, j

!! Test 1

   call initarr1d(a)

   ret = fnt1(a)

   do i = 1, 5
      if ( a(i) /= cmplx(i+1,i+1,C_LONG_DOUBLE_COMPLEX) ) error stop 20
   end do

!! Test 2

   call initarr1d(a)

   ret = fnt2(a)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,C_LONG_DOUBLE_COMPLEX) ) error stop 22
   end do

!! Test 2a

   call initarr1d(a)

   ret = fnt2a(a)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,C_LONG_DOUBLE_COMPLEX) ) error stop 24
   end do

!! Test 3

   call initarr1d(a)

   ret = fnt3(a)

   do i = 1, 5
      if ( a(i) /= cmplx(i+1,i+1,C_LONG_DOUBLE_COMPLEX) ) error stop 26
   end do

!! Test 4

   call initarr1d(a)

   ret = fnt4(a)

   do i = 1, 5
      if ( a(i) /= cmplx(i+1,i+1,C_LONG_DOUBLE_COMPLEX) ) error stop 28
   end do

!! Test 5

   call initarr2d(aa)

   ret = fnt5(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j,i+j,C_LONG_DOUBLE_COMPLEX) ) error stop 30
      end do
   end do

!! Test 6

   call initarr2d(aa)

   ret = fnt6(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,C_LONG_DOUBLE_COMPLEX) ) error stop 32
      end do
   end do

!! Test 6a

   call initarr2d(aa)

   ret = fnt6a(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,C_LONG_DOUBLE_COMPLEX) ) error stop 34
      end do
   end do

!! Test 7

   call initarr2d(aa)

   ret = fnt7(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j,i+j,C_LONG_DOUBLE_COMPLEX) ) error stop 36
      end do
   end do

!! Test 8

   call initarr2d(aa)

   ret = fnt8(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j,i+j,C_LONG_DOUBLE_COMPLEX) ) error stop 38
      end do
   end do

end program fxisoo09

subroutine initarr1d(x)
   use ISO_C_BINDING

   complex(C_LONG_DOUBLE_COMPLEX) :: x(5)

   do i = 1, 5
      x(i) = cmplx(i,i,C_LONG_DOUBLE_COMPLEX)
   end do


end subroutine initarr1d

subroutine initarr2d(xx)
   use ISO_C_BINDING

   complex(C_LONG_DOUBLE_COMPLEX) :: xx(10,5)

   do i = 1, 5
      do j = 1, 10
         xx(j,i) = cmplx(i+j-1,i+j-1,C_LONG_DOUBLE_COMPLEX)
      end do
   end do


end subroutine initarr2d
