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
!*      - FORTRAN code only
!*      - passing derived types with 2-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob27b
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

end module mxisob27b

program fxisoi27b
   use ISO_C_BINDING
   use mxisob27b

   interface
      integer(C_INT_FAST16_T) function fnt1(dt) bind(c)
         use mxisob27b
         type(dtd0), intent(inout) :: dt
      end function fnt1
      integer(C_INT_FAST16_T) function fnt2(dt) bind(c)
         use mxisob27b
         type(dtd0), value :: dt
      end function fnt2
      integer(C_INT_FAST16_T) function fnt3(dt) bind(c)
         use mxisob27b
         type(dtd1), intent(inout) :: dt
      end function fnt3
      integer(C_INT_FAST16_T) function fnt4(dt) bind(c)
         use mxisob27b
         type(dtd1), value :: dt
      end function fnt4
      integer(C_INT_FAST16_T) function fnt5(dt) bind(c)
         use mxisob27b
         type(dtd2), intent(inout) :: dt
      end function fnt5
      integer(C_INT_FAST16_T) function fnt6(dt) bind(c)
         use mxisob27b
         type(dtd2), value :: dt
      end function fnt6
      integer(C_INT_FAST16_T) function fnt7(dt) bind(c)
         use mxisob27b
         type(dtd0), intent(in) :: dt
      end function fnt7
      integer(C_INT_FAST16_T) function fnt8(dt) bind(c)
         use mxisob27b
         type(dtd0), intent(in), value :: dt
      end function fnt8
      integer(C_INT_FAST16_T) function fnt9(dt) bind(c)
         use mxisob27b
         type(dtd1), intent(in) :: dt
      end function fnt9
      integer(C_INT_FAST16_T) function fnt10(dt) bind(c)
         use mxisob27b
         type(dtd1), intent(in), value :: dt
      end function fnt10
      integer(C_INT_FAST16_T) function fnt11(dt) bind(c)
         use mxisob27b
         type(dtd2), intent(in) :: dt
      end function fnt11
      integer(C_INT_FAST16_T) function fnt12(dt) bind(c)
         use mxisob27b
         type(dtd2), intent(in), value :: dt
      end function fnt12
      integer(C_INT_FAST16_T) function fnt13(dt) bind(c)
         use mxisob27b
         type(dtd0), intent(out) :: dt
      end function fnt13
      integer(C_INT_FAST16_T) function fnt14(dt) bind(c)
         use mxisob27b
         type(dtd1), intent(out) :: dt
      end function fnt14
      integer(C_INT_FAST16_T) function fnt15(dt) bind(c)
         use mxisob27b
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
         if ( dta%a(j,i) /= i+j ) error stop 20
      end do
   end do


!! Test 2

   call initdtd0(dta)

   ret = fnt2(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= i+j-1 ) error stop 22
      end do
   end do


!! Test 3

   call initdtd1(dtb)

   ret = fnt3(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j ) error stop 24
         if ( dtb%d0%a(j,i) /= 2*(i+j)-1 ) error stop 26
      end do
   end do


!! Test 4

   call initdtd1(dtb)

   ret = fnt4(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j-1 ) error stop 28
         if ( dtb%d0%a(j,i) /= i+j-1 ) error stop 30
      end do
   end do


!! Test 5

   call initdtd2(dtc)

   ret = fnt5(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j ) error stop 32
         if ( dtc%d1%a(j,i) /= 2*(i+j)-1 ) error stop 34
         if ( dtc%d1%d0%a(j,i) /= 2*(i+j)-1 ) error stop 36
      end do
   end do


!! Test 6

   call initdtd2(dtc)

   ret = fnt6(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j-1 ) error stop 38
         if ( dtc%d1%a(j,i) /= i+j-1 ) error stop 40
         if ( dtc%d1%d0%a(j,i) /= i+j-1 ) error stop 42
      end do
   end do


!! Test 7

   call initdtd0(dta)

   ret = fnt7(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= i+j-1 ) error stop 44
      end do
   end do


!! Test 8

   call initdtd0(dta)

   ret = fnt8(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= i+j-1 ) error stop 46
      end do
   end do


!! Test 9

   call initdtd1(dtb)

   ret = fnt9(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j-1 ) error stop 48
         if ( dtb%d0%a(j,i) /= i+j-1 ) error stop 50
      end do
   end do


!! Test 10

   call initdtd1(dtb)

   ret = fnt10(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j-1 ) error stop 52
         if ( dtb%d0%a(j,i) /= i+j-1 ) error stop 54
      end do
   end do


!! Test 11

   call initdtd2(dtc)

   ret = fnt11(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j-1 ) error stop 56
         if ( dtc%d1%a(j,i) /= i+j-1 ) error stop 58
         if ( dtc%d1%d0%a(j,i) /= i+j-1 ) error stop 60
      end do
   end do


!! Test 12

   call initdtd2(dtc)

   ret = fnt12(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j-1 ) error stop 62
         if ( dtc%d1%a(j,i) /= i+j-1 ) error stop 64
         if ( dtc%d1%d0%a(j,i) /= i+j-1 ) error stop 66
      end do
   end do


!! Test 13

   call initdtd0(dta)

   ret = fnt13(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= i+j ) error stop 68
      end do
   end do


!! Test 14

   call initdtd1(dtb)

   ret = fnt14(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j ) error stop 70
         if ( dtb%d0%a(j,i) /= 2*(i+j)-1 ) error stop 72
      end do
   end do


!! Test 15

   call initdtd2(dtc)

   ret = fnt15(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j ) error stop 74
         if ( dtc%d1%a(j,i) /= 2*(i+j)-1 ) error stop 76
         if ( dtc%d1%d0%a(j,i) /= 2*(i+j)-1 ) error stop 78
      end do
   end do


end program fxisoi27b

subroutine initdtd0(dt)
   use mxisob27b

   type(dtd0) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = i+j-1
      end do
   end do


end subroutine initdtd0

subroutine initdtd1(dt)
   use mxisob27b

   type(dtd1) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = i+j-1
      end do
   end do


   call initdtd0(dt%d0)

end subroutine initdtd1

subroutine initdtd2(dt)
   use mxisob27b

   type(dtd2) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = i+j-1
      end do
   end do


   call initdtd1(dt%d1)

end subroutine initdtd2

integer(C_INT_FAST16_T) function fnt1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd0), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 80
         dt%a(j,i) = i+j
      end do
   end do


   fnt1 = 0
end function fnt1

integer(C_INT_FAST16_T) function fnt2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd0), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 82
         dt%a(j,i) = i+j
      end do
   end do


   fnt2 = 0
end function fnt2

integer(C_INT_FAST16_T) function fnt3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd1), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 84
         dt%a(j,i) = i+j
         if ( dt%d0%a(j,i) /= i+j-1 ) error stop 86
         dt%d0%a(j,i) = dt%d0%a(j,i)+i+j
      end do
   end do


   fnt3 = 0
end function fnt3

integer(C_INT_FAST16_T) function fnt4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd1), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 88
         dt%a(j,i) = i+j
         if ( dt%d0%a(j,i) /= i+j-1 ) error stop 90
         dt%d0%a(j,i) = dt%d0%a(j,i)+i+j
      end do
   end do


   fnt4 = 0
end function fnt4

integer(C_INT_FAST16_T) function fnt5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd2), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 92
         dt%a(j,i) = i+j
         if ( dt%d1%a(j,i) /= i+j-1 ) error stop 94
         dt%d1%a(j,i) = dt%d1%a(j,i)+i+j
         if ( dt%d1%d0%a(j,i) /= i+j-1 ) error stop 96
         dt%d1%d0%a(j,i) = dt%d1%d0%a(j,i)+i+j
      end do
   end do


   fnt5 = 0
end function fnt5

integer(C_INT_FAST16_T) function fnt6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd2), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 98
         dt%a(j,i) = i+j
         if ( dt%d1%a(j,i) /= i+j-1 ) error stop 100
         dt%d1%a(j,i) = dt%d1%a(j,i)+i+j
         if ( dt%d1%d0%a(j,i) /= i+j-1 ) error stop 102
         dt%d1%d0%a(j,i) = dt%d1%d0%a(j,i)+i+j
      end do
   end do


   fnt6 = 0
end function fnt6

integer(C_INT_FAST16_T) function fnt7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd0), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 104
      end do
   end do


   fnt7 = 0
end function fnt7

integer(C_INT_FAST16_T) function fnt8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd0), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 106
      end do
   end do


   fnt8 = 0
end function fnt8

integer(C_INT_FAST16_T) function fnt9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd1), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 108
         if ( dt%d0%a(j,i) /= i+j-1 ) error stop 110
      end do
   end do


   fnt9 = 0
end function fnt9

integer(C_INT_FAST16_T) function fnt10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd1), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 112
         if ( dt%d0%a(j,i) /= i+j-1 ) error stop 114
      end do
   end do


   fnt10 = 0
end function fnt10

integer(C_INT_FAST16_T) function fnt11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd2), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 116
         if ( dt%d1%a(j,i) /= i+j-1 ) error stop 118
         if ( dt%d1%d0%a(j,i) /= i+j-1 ) error stop 120
      end do
   end do


   fnt11 = 0
end function fnt11

integer(C_INT_FAST16_T) function fnt12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd2), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 122
         if ( dt%d1%a(j,i) /= i+j-1 ) error stop 124
         if ( dt%d1%d0%a(j,i) /= i+j-1 ) error stop 126
      end do
   end do


   fnt12 = 0
end function fnt12

integer(C_INT_FAST16_T) function fnt13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd0), intent(out) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = i+j
      end do
   end do


   fnt13 = 0
end function fnt13

integer(C_INT_FAST16_T) function fnt14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd1), intent(out) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = i+j
         dt%d0%a(j,i) = dt%d0%a(j,i)+i+j
      end do
   end do


   fnt14 = 0
end function fnt14

integer(C_INT_FAST16_T) function fnt15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd2), intent(out) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = i+j
         dt%d1%a(j,i) = dt%d1%a(j,i)+i+j
         dt%d1%d0%a(j,i) = dt%d1%d0%a(j,i)+i+j
      end do
   end do


   fnt15 = 0
end function fnt15
