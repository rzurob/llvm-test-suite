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
!*  KEYWORD(S)                 : C_INT_FAST16_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_INT_FAST16_T
!*      - using C functions with interfaces to FORTRAN subroutines
!*      - passing derived types with 2-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob22b
   use ISO_C_BINDING

   type, bind(c) :: dtd0
      integer(C_INT_FAST16_T) :: a(10,5)
   end type

   type, bind(c) :: dtd1
      integer(C_INT_FAST16_T) :: a(10,5)
      type(dtd0) :: d0
   end type

   type, bind(c) :: dtd2
      integer(C_INT_FAST16_T) :: a(10,5)
      type(dtd1) :: d1
   end type

end module mxisob22b

program fxisoi22b
   use ISO_C_BINDING
   use mxisob22b

   interface
      subroutine sub1(dt) bind(c)
         use mxisob22b
         type(dtd0), intent(inout) :: dt
      end subroutine sub1
      subroutine sub2(dt) bind(c)
         use mxisob22b
         type(dtd0), value :: dt
      end subroutine sub2
      subroutine sub3(dt) bind(c)
         use mxisob22b
         type(dtd1), intent(inout) :: dt
      end subroutine sub3
      subroutine sub4(dt) bind(c)
         use mxisob22b
         type(dtd1), value :: dt
      end subroutine sub4
      subroutine sub5(dt) bind(c)
         use mxisob22b
         type(dtd2), intent(inout) :: dt
      end subroutine sub5
      subroutine sub6(dt) bind(c)
         use mxisob22b
         type(dtd2), value :: dt
      end subroutine sub6
      subroutine sub7(dt) bind(c)
         use mxisob22b
         type(dtd0), intent(in) :: dt
      end subroutine sub7
      subroutine sub7a(dt) bind(c)
         use mxisob22b
         type(dtd0), intent(in) :: dt
      end subroutine sub7a
      subroutine sub8(dt) bind(c)
         use mxisob22b
         type(dtd0), intent(in), value :: dt
      end subroutine sub8
      subroutine sub8a(dt) bind(c)
         use mxisob22b
         type(dtd0), intent(in), value :: dt
      end subroutine sub8a
      subroutine sub9(dt) bind(c)
         use mxisob22b
         type(dtd1), intent(in) :: dt
      end subroutine sub9
      subroutine sub9a(dt) bind(c)
         use mxisob22b
         type(dtd1), intent(in) :: dt
      end subroutine sub9a
      subroutine sub10(dt) bind(c)
         use mxisob22b
         type(dtd1), intent(in), value :: dt
      end subroutine sub10
      subroutine sub10a(dt) bind(c)
         use mxisob22b
         type(dtd1), intent(in), value :: dt
      end subroutine sub10a
      subroutine sub11(dt) bind(c)
         use mxisob22b
         type(dtd2), intent(in) :: dt
      end subroutine sub11
      subroutine sub11a(dt) bind(c)
         use mxisob22b
         type(dtd2), intent(in) :: dt
      end subroutine sub11a
      subroutine sub12(dt) bind(c)
         use mxisob22b
         type(dtd2), intent(in), value :: dt
      end subroutine sub12
      subroutine sub12a(dt) bind(c)
         use mxisob22b
         type(dtd2), intent(in), value :: dt
      end subroutine sub12a
      subroutine sub13(dt) bind(c)
         use mxisob22b
         type(dtd0), intent(out) :: dt
      end subroutine sub13
      subroutine sub14(dt) bind(c)
         use mxisob22b
         type(dtd1), intent(out) :: dt
      end subroutine sub14
      subroutine sub15(dt) bind(c)
         use mxisob22b
         type(dtd2), intent(out) :: dt
      end subroutine sub15
   end interface

   type(dtd0) :: dta
   type(dtd1) :: dtb
   type(dtd2) :: dtc
   integer i, j, ret

!! Test 1

   call initdtd0(dta)

   call sub1(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= i+j ) error stop 20
      end do
   end do


!! Test 2

   call initdtd0(dta)

   call sub2(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= i+j-1 ) error stop 22
      end do
   end do


!! Test 3

   call initdtd1(dtb)

   call sub3(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j ) error stop 24
         if ( dtb%d0%a(j,i) /= i+j ) error stop 26
      end do
   end do


!! Test 4

   call initdtd1(dtb)

   call sub4(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j-1 ) error stop 28
         if ( dtb%d0%a(j,i) /= i+j-1 ) error stop 30
      end do
   end do


!! Test 5

   call initdtd2(dtc)

   call sub5(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j ) error stop 32
         if ( dtc%d1%a(j,i) /= i+j ) error stop 34
         if ( dtc%d1%d0%a(j,i) /= i+j ) error stop 36
      end do
   end do


!! Test 6

   call initdtd2(dtc)

   call sub6(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j-1 ) error stop 38
         if ( dtc%d1%a(j,i) /= i+j-1 ) error stop 40
         if ( dtc%d1%d0%a(j,i) /= i+j-1 ) error stop 42
      end do
   end do


!! Test 7

   call initdtd0(dta)

   call sub7(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= i+j-1 ) error stop 44
      end do
   end do


!! Test 7a

   call initdtd0(dta)

   call sub7a(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= i+j-1 ) error stop 46
      end do
   end do


!! Test 8

   call initdtd0(dta)

   call sub8(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= i+j-1 ) error stop 48
      end do
   end do


!! Test 8a

   call initdtd0(dta)

   call sub8a(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= i+j-1 ) error stop 50
      end do
   end do


!! Test 9

   call initdtd1(dtb)

   call sub9(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j-1 ) error stop 52
         if ( dtb%d0%a(j,i) /= i+j-1 ) error stop 54
      end do
   end do


!! Test 9a

   call initdtd1(dtb)

   call sub9a(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j-1 ) error stop 56
         if ( dtb%d0%a(j,i) /= i+j-1 ) error stop 58
      end do
   end do


!! Test 10

   call initdtd1(dtb)

   call sub10(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j-1 ) error stop 60
         if ( dtb%d0%a(j,i) /= i+j-1 ) error stop 62
      end do
   end do


!! Test 10a

   call initdtd1(dtb)

   call sub10a(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j-1 ) error stop 64
         if ( dtb%d0%a(j,i) /= i+j-1 ) error stop 66
      end do
   end do


!! Test 11

   call initdtd2(dtc)

   call sub11(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j-1 ) error stop 68
         if ( dtc%d1%a(j,i) /= i+j-1 ) error stop 70
         if ( dtc%d1%d0%a(j,i) /= i+j-1 ) error stop 72
      end do
   end do


!! Test 11a

   call initdtd2(dtc)

   call sub11a(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j-1 ) error stop 74
         if ( dtc%d1%a(j,i) /= i+j-1 ) error stop 76
         if ( dtc%d1%d0%a(j,i) /= i+j-1 ) error stop 78
      end do
   end do


!! Test 12

   call initdtd2(dtc)

   call sub12(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j-1 ) error stop 80
         if ( dtc%d1%a(j,i) /= i+j-1 ) error stop 82
         if ( dtc%d1%d0%a(j,i) /= i+j-1 ) error stop 84
      end do
   end do


!! Test 12a

   call initdtd2(dtc)

   call sub12a(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j-1 ) error stop 86
         if ( dtc%d1%a(j,i) /= i+j-1 ) error stop 88
         if ( dtc%d1%d0%a(j,i) /= i+j-1 ) error stop 90
      end do
   end do


!! Test 13

   call initdtd0(dta)

   call sub13(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= i+j ) error stop 92
      end do
   end do


!! Test 14

   call initdtd1(dtb)

   call sub14(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j ) error stop 94
         if ( dtb%d0%a(j,i) /= i+j ) error stop 96
      end do
   end do


!! Test 15

   call initdtd2(dtc)

   call sub15(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j ) error stop 98
         if ( dtc%d1%a(j,i) /= i+j ) error stop 100
         if ( dtc%d1%d0%a(j,i) /= i+j ) error stop 102
      end do
   end do


end program fxisoi22b

subroutine initdtd0(dt)
   use mxisob22b

   type(dtd0) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = i+j-1
      end do
   end do


end subroutine initdtd0

subroutine initdtd1(dt)
   use mxisob22b

   type(dtd1) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = i+j-1
      end do
   end do


   call initdtd0(dt%d0)

end subroutine initdtd1

subroutine initdtd2(dt)
   use mxisob22b

   type(dtd2) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = i+j-1
      end do
   end do


   call initdtd1(dt%d1)

end subroutine initdtd2
