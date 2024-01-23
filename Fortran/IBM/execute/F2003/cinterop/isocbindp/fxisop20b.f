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
!*      - using C functions with interfaces to FORTRAN functions
!*      - passing derived types with 2-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob20b
   use ISO_C_BINDING

   type, bind(c) :: dtd0
      real(16) :: a(10,5)
   end type

   type, bind(c) :: dtd1
      real(16) :: a(10,5)
      type(dtd0) :: d0
   end type

   type, bind(c) :: dtd2
      real(16) :: a(10,5)
      type(dtd1) :: d1
   end type

end module mxisob20b

program fxisop20b
   use ISO_C_BINDING
   use mxisob20b

   interface
      real(16) function fnt1(dt) bind(c)
         use mxisob20b
         type(dtd0), intent(inout) :: dt
      end function fnt1
      real(16) function fnt2(dt) bind(c)
         use mxisob20b
         type(dtd0), value :: dt
      end function fnt2
      real(16) function fnt3(dt) bind(c)
         use mxisob20b
         type(dtd1), intent(inout) :: dt
      end function fnt3
      real(16) function fnt4(dt) bind(c)
         use mxisob20b
         type(dtd1), value :: dt
      end function fnt4
      real(16) function fnt5(dt) bind(c)
         use mxisob20b
         type(dtd2), intent(inout) :: dt
      end function fnt5
      real(16) function fnt6(dt) bind(c)
         use mxisob20b
         type(dtd2), value :: dt
      end function fnt6
      real(16) function fnt7(dt) bind(c)
         use mxisob20b
         type(dtd0), intent(in) :: dt
      end function fnt7
      real(16) function fnt7a(dt) bind(c)
         use mxisob20b
         type(dtd0), intent(in) :: dt
      end function fnt7a
      real(16) function fnt8(dt) bind(c)
         use mxisob20b
         type(dtd0), intent(in), value :: dt
      end function fnt8
      real(16) function fnt8a(dt) bind(c)
         use mxisob20b
         type(dtd0), intent(in), value :: dt
      end function fnt8a
      real(16) function fnt9(dt) bind(c)
         use mxisob20b
         type(dtd1), intent(in) :: dt
      end function fnt9
      real(16) function fnt9a(dt) bind(c)
         use mxisob20b
         type(dtd1), intent(in) :: dt
      end function fnt9a
      real(16) function fnt10(dt) bind(c)
         use mxisob20b
         type(dtd1), intent(in), value :: dt
      end function fnt10
      real(16) function fnt10a(dt) bind(c)
         use mxisob20b
         type(dtd1), intent(in), value :: dt
      end function fnt10a
      real(16) function fnt11(dt) bind(c)
         use mxisob20b
         type(dtd2), intent(in) :: dt
      end function fnt11
      real(16) function fnt11a(dt) bind(c)
         use mxisob20b
         type(dtd2), intent(in) :: dt
      end function fnt11a
      real(16) function fnt12(dt) bind(c)
         use mxisob20b
         type(dtd2), intent(in), value :: dt
      end function fnt12
      real(16) function fnt12a(dt) bind(c)
         use mxisob20b
         type(dtd2), intent(in), value :: dt
      end function fnt12a
      real(16) function fnt13(dt) bind(c)
         use mxisob20b
         type(dtd0), intent(out) :: dt
      end function fnt13
      real(16) function fnt14(dt) bind(c)
         use mxisob20b
         type(dtd1), intent(out) :: dt
      end function fnt14
      real(16) function fnt15(dt) bind(c)
         use mxisob20b
         type(dtd2), intent(out) :: dt
      end function fnt15
   end interface

   type(dtd0) :: dta
   type(dtd1) :: dtb
   type(dtd2) :: dtc
   integer i, j, ret

!! Test 1

   call initdtd0(dta)

   ret = fnt1(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= real(i+j,16) ) error stop 20
      end do
   end do


!! Test 2

   call initdtd0(dta)

   ret = fnt2(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= real(i+j-1,16) ) error stop 22
      end do
   end do


!! Test 3

   call initdtd1(dtb)

   ret = fnt3(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= real(i+j,16) ) error stop 24
         if ( dtb%d0%a(j,i) /= real(i+j,16) ) error stop 26
      end do
   end do


!! Test 4

   call initdtd1(dtb)

   ret = fnt4(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= real(i+j-1,16) ) error stop 28
         if ( dtb%d0%a(j,i) /= real(i+j-1,16) ) error stop 30
      end do
   end do


!! Test 5

   call initdtd2(dtc)

   ret = fnt5(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= real(i+j,16) ) error stop 32
         if ( dtc%d1%a(j,i) /= real(i+j,16) ) error stop 34
         if ( dtc%d1%d0%a(j,i) /= real(i+j,16) ) error stop 36
      end do
   end do


!! Test 6

   call initdtd2(dtc)

   ret = fnt6(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= real(i+j-1,16) ) error stop 38
         if ( dtc%d1%a(j,i) /= real(i+j-1,16) ) error stop 40
         if ( dtc%d1%d0%a(j,i) /= real(i+j-1,16) ) error stop 42
      end do
   end do


!! Test 7

   call initdtd0(dta)

   ret = fnt7(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= real(i+j-1,16) ) error stop 44
      end do
   end do


!! Test 7a

   call initdtd0(dta)

   ret = fnt7a(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= real(i+j-1,16) ) error stop 46
      end do
   end do


!! Test 8

   call initdtd0(dta)

   ret = fnt8(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= real(i+j-1,16) ) error stop 48
      end do
   end do


!! Test 8a

   call initdtd0(dta)

   ret = fnt8a(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= real(i+j-1,16) ) error stop 50
      end do
   end do


!! Test 9

   call initdtd1(dtb)

   ret = fnt9(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= real(i+j-1,16) ) error stop 52
         if ( dtb%d0%a(j,i) /= real(i+j-1,16) ) error stop 54
      end do
   end do


!! Test 9a

   call initdtd1(dtb)

   ret = fnt9a(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= real(i+j-1,16) ) error stop 56
         if ( dtb%d0%a(j,i) /= real(i+j-1,16) ) error stop 58
      end do
   end do


!! Test 10

   call initdtd1(dtb)

   ret = fnt10(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= real(i+j-1,16) ) error stop 60
         if ( dtb%d0%a(j,i) /= real(i+j-1,16) ) error stop 62
      end do
   end do


!! Test 10a

   call initdtd1(dtb)

   ret = fnt10a(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= real(i+j-1,16) ) error stop 64
         if ( dtb%d0%a(j,i) /= real(i+j-1,16) ) error stop 66
      end do
   end do


!! Test 11

   call initdtd2(dtc)

   ret = fnt11(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= real(i+j-1,16) ) error stop 68
         if ( dtc%d1%a(j,i) /= real(i+j-1,16) ) error stop 70
         if ( dtc%d1%d0%a(j,i) /= real(i+j-1,16) ) error stop 72
      end do
   end do


!! Test 11a

   call initdtd2(dtc)

   ret = fnt11a(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= real(i+j-1,16) ) error stop 74
         if ( dtc%d1%a(j,i) /= real(i+j-1,16) ) error stop 76
         if ( dtc%d1%d0%a(j,i) /= real(i+j-1,16) ) error stop 78
      end do
   end do


!! Test 12

   call initdtd2(dtc)

   ret = fnt12(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= real(i+j-1,16) ) error stop 80
         if ( dtc%d1%a(j,i) /= real(i+j-1,16) ) error stop 82
         if ( dtc%d1%d0%a(j,i) /= real(i+j-1,16) ) error stop 84
      end do
   end do


!! Test 12a

   call initdtd2(dtc)

   ret = fnt12a(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= real(i+j-1,16) ) error stop 86
         if ( dtc%d1%a(j,i) /= real(i+j-1,16) ) error stop 88
         if ( dtc%d1%d0%a(j,i) /= real(i+j-1,16) ) error stop 90
      end do
   end do


!! Test 13

   call initdtd0(dta)

   ret = fnt13(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= real(i+j,16) ) error stop 92
      end do
   end do


!! Test 14

   call initdtd1(dtb)

   ret = fnt14(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= real(i+j,16) ) error stop 94
         if ( dtb%d0%a(j,i) /= real(i+j,16) ) error stop 96
      end do
   end do


!! Test 15

   call initdtd2(dtc)

   ret = fnt15(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= real(i+j,16) ) error stop 98
         if ( dtc%d1%a(j,i) /= real(i+j,16) ) error stop 100
         if ( dtc%d1%d0%a(j,i) /= real(i+j,16) ) error stop 102
      end do
   end do


end program fxisop20b

subroutine initdtd0(dt)
   use mxisob20b

   type(dtd0) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = real(i+j-1,16)
      end do
   end do


end subroutine initdtd0

subroutine initdtd1(dt)
   use mxisob20b

   type(dtd1) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = real(i+j-1,16)
      end do
   end do


   call initdtd0(dt%d0)

end subroutine initdtd1

subroutine initdtd2(dt)
   use mxisob20b

   type(dtd2) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = real(i+j-1,16)
      end do
   end do


   call initdtd1(dt%d1)

end subroutine initdtd2
