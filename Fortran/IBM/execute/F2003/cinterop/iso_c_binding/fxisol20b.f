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
!*  KEYWORD(S)                 : C_FLOAT, C_DOUBLE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_FLOAT and C_DOUBLE
!*      - using C functions with interfaces to FORTRAN functions
!*      - passing derived types with 2-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob20b
   use ISO_C_BINDING

   type, bind(c) :: dtd0
      real(C_FLOAT) :: a(10,5)
      real(C_DOUBLE) :: b(6,3)
   end type

   type, bind(c) :: dtd1
      real(C_FLOAT) :: a(10,5)
      real(C_DOUBLE) :: b(6,3)
      type(dtd0) :: d0
   end type

   type, bind(c) :: dtd2
      real(C_FLOAT) :: a(10,5)
      real(C_DOUBLE) :: b(6,3)
      type(dtd1) :: d1
   end type

end module mxisob20b

program fxisol20b
   use ISO_C_BINDING
   use mxisob20b

   interface
      real(C_FLOAT) function fnt1(dt) bind(c)
         use mxisob20b
         type(dtd0), intent(inout) :: dt
      end function fnt1
      real(C_FLOAT) function fnt2(dt) bind(c)
         use mxisob20b
         type(dtd0), value :: dt
      end function fnt2
      real(C_FLOAT) function fnt3(dt) bind(c)
         use mxisob20b
         type(dtd1), intent(inout) :: dt
      end function fnt3
      real(C_FLOAT) function fnt4(dt) bind(c)
         use mxisob20b
         type(dtd1), value :: dt
      end function fnt4
      real(C_FLOAT) function fnt5(dt) bind(c)
         use mxisob20b
         type(dtd2), intent(inout) :: dt
      end function fnt5
      real(C_FLOAT) function fnt6(dt) bind(c)
         use mxisob20b
         type(dtd2), value :: dt
      end function fnt6
      real(C_FLOAT) function fnt7(dt) bind(c)
         use mxisob20b
         type(dtd0), intent(in) :: dt
      end function fnt7
      real(C_FLOAT) function fnt7a(dt) bind(c)
         use mxisob20b
         type(dtd0), intent(in) :: dt
      end function fnt7a
      real(C_FLOAT) function fnt8(dt) bind(c)
         use mxisob20b
         type(dtd0), intent(in), value :: dt
      end function fnt8
      real(C_FLOAT) function fnt8a(dt) bind(c)
         use mxisob20b
         type(dtd0), intent(in), value :: dt
      end function fnt8a
      real(C_FLOAT) function fnt9(dt) bind(c)
         use mxisob20b
         type(dtd1), intent(in) :: dt
      end function fnt9
      real(C_FLOAT) function fnt9a(dt) bind(c)
         use mxisob20b
         type(dtd1), intent(in) :: dt
      end function fnt9a
      real(C_FLOAT) function fnt10(dt) bind(c)
         use mxisob20b
         type(dtd1), intent(in), value :: dt
      end function fnt10
      real(C_FLOAT) function fnt10a(dt) bind(c)
         use mxisob20b
         type(dtd1), intent(in), value :: dt
      end function fnt10a
      real(C_FLOAT) function fnt11(dt) bind(c)
         use mxisob20b
         type(dtd2), intent(in) :: dt
      end function fnt11
      real(C_FLOAT) function fnt11a(dt) bind(c)
         use mxisob20b
         type(dtd2), intent(in) :: dt
      end function fnt11a
      real(C_FLOAT) function fnt12(dt) bind(c)
         use mxisob20b
         type(dtd2), intent(in), value :: dt
      end function fnt12
      real(C_FLOAT) function fnt12a(dt) bind(c)
         use mxisob20b
         type(dtd2), intent(in), value :: dt
      end function fnt12a
      real(C_FLOAT) function fnt13(dt) bind(c)
         use mxisob20b
         type(dtd0), intent(out) :: dt
      end function fnt13
      real(C_FLOAT) function fnt14(dt) bind(c)
         use mxisob20b
         type(dtd1), intent(out) :: dt
      end function fnt14
      real(C_FLOAT) function fnt15(dt) bind(c)
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
         if ( dta%a(j,i) /= real(i+j,C_FLOAT) ) error stop 20
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dta%b(j,i) /= real(i+j,C_DOUBLE) ) error stop 22
      end do
   end do

!! Test 2

   call initdtd0(dta)

   ret = fnt2(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 24
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dta%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 26
      end do
   end do

!! Test 3

   call initdtd1(dtb)

   ret = fnt3(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= real(i+j,C_FLOAT) ) error stop 28
         if ( dtb%d0%a(j,i) /= real(i+j,C_FLOAT) ) error stop 30
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtb%b(j,i) /= real(i+j,C_DOUBLE) ) error stop 32
         if ( dtb%d0%b(j,i) /= real(i+j,C_DOUBLE) ) error stop 34
      end do
   end do

!! Test 4

   call initdtd1(dtb)

   ret = fnt4(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 36
         if ( dtb%d0%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 38
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtb%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 40
         if ( dtb%d0%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 42
      end do
   end do

!! Test 5

   call initdtd2(dtc)

   ret = fnt5(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= real(i+j,C_FLOAT) ) error stop 44
         if ( dtc%d1%a(j,i) /= real(i+j,C_FLOAT) ) error stop 46
         if ( dtc%d1%d0%a(j,i) /= real(i+j,C_FLOAT) ) error stop 48
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtc%b(j,i) /= real(i+j,C_DOUBLE) ) error stop 50
         if ( dtc%d1%b(j,i) /= real(i+j,C_DOUBLE) ) error stop 52
         if ( dtc%d1%d0%b(j,i) /= real(i+j,C_DOUBLE) ) error stop 54
      end do
   end do

!! Test 6

   call initdtd2(dtc)

   ret = fnt6(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 56
         if ( dtc%d1%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 58
         if ( dtc%d1%d0%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 60
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtc%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 62
         if ( dtc%d1%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 64
         if ( dtc%d1%d0%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 66
      end do
   end do

!! Test 7

   call initdtd0(dta)

   ret = fnt7(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 68
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dta%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 70
      end do
   end do

!! Test 7a

   call initdtd0(dta)

   ret = fnt7a(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 72
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dta%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 74
      end do
   end do

!! Test 8

   call initdtd0(dta)

   ret = fnt8(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 76
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dta%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 78
      end do
   end do

!! Test 8a

   call initdtd0(dta)

   ret = fnt8a(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 80
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dta%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 82
      end do
   end do

!! Test 9

   call initdtd1(dtb)

   ret = fnt9(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 84
         if ( dtb%d0%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 86
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtb%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 88
         if ( dtb%d0%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 90
      end do
   end do

!! Test 9a

   call initdtd1(dtb)

   ret = fnt9a(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 92
         if ( dtb%d0%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 94
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtb%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 96
         if ( dtb%d0%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 98
      end do
   end do

!! Test 10

   call initdtd1(dtb)

   ret = fnt10(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 100
         if ( dtb%d0%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 102
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtb%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 104
         if ( dtb%d0%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 106
      end do
   end do

!! Test 10a

   call initdtd1(dtb)

   ret = fnt10a(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 108
         if ( dtb%d0%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 110
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtb%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 112
         if ( dtb%d0%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 114
      end do
   end do

!! Test 11

   call initdtd2(dtc)

   ret = fnt11(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 116
         if ( dtc%d1%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 118
         if ( dtc%d1%d0%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 120
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtc%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 122
         if ( dtc%d1%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 124
         if ( dtc%d1%d0%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 126
      end do
   end do

!! Test 11a

   call initdtd2(dtc)

   ret = fnt11a(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 128
         if ( dtc%d1%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 130
         if ( dtc%d1%d0%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 132
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtc%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 134
         if ( dtc%d1%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 136
         if ( dtc%d1%d0%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 138
      end do
   end do

!! Test 12

   call initdtd2(dtc)

   ret = fnt12(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 140
         if ( dtc%d1%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 142
         if ( dtc%d1%d0%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 144
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtc%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 146
         if ( dtc%d1%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 148
         if ( dtc%d1%d0%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 150
      end do
   end do

!! Test 12a

   call initdtd2(dtc)

   ret = fnt12a(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 152
         if ( dtc%d1%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 154
         if ( dtc%d1%d0%a(j,i) /= real(i+j-1,C_FLOAT) ) error stop 156
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtc%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 158
         if ( dtc%d1%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 160
         if ( dtc%d1%d0%b(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 162
      end do
   end do

!! Test 13

   call initdtd0(dta)

   ret = fnt13(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= real(i+j,C_FLOAT) ) error stop 164
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dta%b(j,i) /= real(i+j,C_DOUBLE) ) error stop 166
      end do
   end do

!! Test 14

   call initdtd1(dtb)

   ret = fnt14(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= real(i+j,C_FLOAT) ) error stop 168
         if ( dtb%d0%a(j,i) /= real(i+j,C_FLOAT) ) error stop 170
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtb%b(j,i) /= real(i+j,C_DOUBLE) ) error stop 172
         if ( dtb%d0%b(j,i) /= real(i+j,C_DOUBLE) ) error stop 174
      end do
   end do

!! Test 15

   call initdtd2(dtc)

   ret = fnt15(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= real(i+j,C_FLOAT) ) error stop 176
         if ( dtc%d1%a(j,i) /= real(i+j,C_FLOAT) ) error stop 178
         if ( dtc%d1%d0%a(j,i) /= real(i+j,C_FLOAT) ) error stop 180
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtc%b(j,i) /= real(i+j,C_DOUBLE) ) error stop 182
         if ( dtc%d1%b(j,i) /= real(i+j,C_DOUBLE) ) error stop 184
         if ( dtc%d1%d0%b(j,i) /= real(i+j,C_DOUBLE) ) error stop 186
      end do
   end do

end program fxisol20b

subroutine initdtd0(dt)
   use mxisob20b

   type(dtd0) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = real(i+j-1,C_FLOAT)
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         dt%b(j,i) = real(i+j-1,C_DOUBLE)
      end do
   end do

end subroutine initdtd0

subroutine initdtd1(dt)
   use mxisob20b

   type(dtd1) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = real(i+j-1,C_FLOAT)
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         dt%b(j,i) = real(i+j-1,C_DOUBLE)
      end do
   end do

   call initdtd0(dt%d0)

end subroutine initdtd1

subroutine initdtd2(dt)
   use mxisob20b

   type(dtd2) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = real(i+j-1,C_FLOAT)
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         dt%b(j,i) = real(i+j-1,C_DOUBLE)
      end do
   end do

   call initdtd1(dt%d1)

end subroutine initdtd2
