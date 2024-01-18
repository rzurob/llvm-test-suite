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
!*      - using C functions with interfaces to FORTRAN functions
!*      - passing derived types with 1-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob20a
   use ISO_C_BINDING

   type, bind(c) :: dts0
      character(C_CHAR) :: a(4)
      integer(C_SIGNED_CHAR) :: b(3)
   end type

   type, bind(c) :: dts1
      character(C_CHAR) :: a(4)
      integer(C_SIGNED_CHAR) :: b(3)
      type(dts0) :: d0
   end type

   type, bind(c) :: dts2
      character(C_CHAR) :: a(4)
      integer(C_SIGNED_CHAR) :: b(3)
      type(dts1) :: d1
   end type

end module mxisob20a

program fxisok20a
   use ISO_C_BINDING
   use mxisob20a

   interface
      integer(C_SIGNED_CHAR) function fnt1(dt) bind(c)
         use mxisob20a
         type(dts0), intent(inout) :: dt
      end function fnt1
      integer(C_SIGNED_CHAR) function fnt2(dt) bind(c)
         use mxisob20a
         type(dts0), value :: dt
      end function fnt2
      integer(C_SIGNED_CHAR) function fnt3(dt) bind(c)
         use mxisob20a
         type(dts1), intent(inout) :: dt
      end function fnt3
      integer(C_SIGNED_CHAR) function fnt4(dt) bind(c)
         use mxisob20a
         type(dts1), value :: dt
      end function fnt4
      integer(C_SIGNED_CHAR) function fnt5(dt) bind(c)
         use mxisob20a
         type(dts2), intent(inout) :: dt
      end function fnt5
      integer(C_SIGNED_CHAR) function fnt6(dt) bind(c)
         use mxisob20a
         type(dts2), value :: dt
      end function fnt6
      integer(C_SIGNED_CHAR) function fnt7(dt) bind(c)
         use mxisob20a
         type(dts0), intent(in) :: dt
      end function fnt7
      integer(C_SIGNED_CHAR) function fnt7a(dt) bind(c)
         use mxisob20a
         type(dts0), intent(in) :: dt
      end function fnt7a
      integer(C_SIGNED_CHAR) function fnt8(dt) bind(c)
         use mxisob20a
         type(dts0), intent(in), value :: dt
      end function fnt8
      integer(C_SIGNED_CHAR) function fnt8a(dt) bind(c)
         use mxisob20a
         type(dts0), intent(in), value :: dt
      end function fnt8a
      integer(C_SIGNED_CHAR) function fnt9(dt) bind(c)
         use mxisob20a
         type(dts1), intent(in) :: dt
      end function fnt9
      integer(C_SIGNED_CHAR) function fnt9a(dt) bind(c)
         use mxisob20a
         type(dts1), intent(in) :: dt
      end function fnt9a
      integer(C_SIGNED_CHAR) function fnt10(dt) bind(c)
         use mxisob20a
         type(dts1), intent(in), value :: dt
      end function fnt10
      integer(C_SIGNED_CHAR) function fnt10a(dt) bind(c)
         use mxisob20a
         type(dts1), intent(in), value :: dt
      end function fnt10a
      integer(C_SIGNED_CHAR) function fnt11(dt) bind(c)
         use mxisob20a
         type(dts2), intent(in) :: dt
      end function fnt11
      integer(C_SIGNED_CHAR) function fnt11a(dt) bind(c)
         use mxisob20a
         type(dts2), intent(in) :: dt
      end function fnt11a
      integer(C_SIGNED_CHAR) function fnt12(dt) bind(c)
         use mxisob20a
         type(dts2), intent(in), value :: dt
      end function fnt12
      integer(C_SIGNED_CHAR) function fnt12a(dt) bind(c)
         use mxisob20a
         type(dts2), intent(in), value :: dt
      end function fnt12a
      integer(C_SIGNED_CHAR) function fnt13(dt) bind(c)
         use mxisob20a
         type(dts0), intent(out) :: dt
      end function fnt13
      integer(C_SIGNED_CHAR) function fnt14(dt) bind(c)
         use mxisob20a
         type(dts1), intent(out) :: dt
      end function fnt14
      integer(C_SIGNED_CHAR) function fnt15(dt) bind(c)
         use mxisob20a
         type(dts2), intent(out) :: dt
      end function fnt15
   end interface

   type(dts0) :: dta
   type(dts1) :: dtb
   type(dts2) :: dtc
   integer ret

!! Test 1

   call initdts0(dta)

   ret = fnt1(dta)

   do i = 1, 4
      if ( dta%a(i) /= achar(iachar('A')+i+3) ) error stop 20
   end do

   do i = 1, 3
      if ( dta%b(i) /= iachar('A')+i+3 ) error stop 22
   end do

!! Test 2

   call initdts0(dta)

   ret = fnt2(dta)

   do i = 1, 4
      if ( dta%a(i) /= achar(iachar('A')+i-1) ) error stop 24
   end do

   do i = 1, 3
      if ( dta%b(i) /= iachar('A')+i-1 ) error stop 26
   end do

!! Test 3

   call initdts1(dtb)

   ret = fnt3(dtb)

   do i = 1, 4
      if ( dtb%a(i) /= achar(iachar('A')+i+3) ) error stop 28
      if ( dtb%d0%a(i) /= achar(iachar('A')+i+3) ) error stop 30
   end do

   do i = 1, 3
      if ( dtb%b(i) /= iachar('A')+i+3 ) error stop 32
      if ( dtb%d0%b(i) /= iachar('A')+i+3 ) error stop 34
   end do

!! Test 4

   call initdts1(dtb)

   ret = fnt4(dtb)

   do i = 1, 4
      if ( dtb%a(i) /= achar(iachar('A')+i-1) ) error stop 36
      if ( dtb%d0%a(i) /= achar(iachar('A')+i-1) ) error stop 38
   end do

   do i = 1, 3
      if ( dtb%b(i) /= iachar('A')+i-1 ) error stop 40
      if ( dtb%d0%b(i) /= iachar('A')+i-1 ) error stop 42
   end do

!! Test 5

   call initdts2(dtc)

   ret = fnt5(dtc)

   do i = 1, 4
      if ( dtc%a(i) /= achar(iachar('A')+i+3) ) error stop 44
      if ( dtc%d1%a(i) /= achar(iachar('A')+i+3) ) error stop 46
      if ( dtc%d1%d0%a(i) /= achar(iachar('A')+i+3) ) error stop 48
   end do

   do i = 1, 3
      if ( dtc%b(i) /= iachar('A')+i+3 ) error stop 50
      if ( dtc%d1%b(i) /= iachar('A')+i+3 ) error stop 52
      if ( dtc%d1%d0%b(i) /= iachar('A')+i+3 ) error stop 54
   end do

!! Test 6

   call initdts2(dtc)

   ret = fnt6(dtc)

   do i = 1, 4
      if ( dtc%a(i) /= achar(iachar('A')+i-1) ) error stop 56
      if ( dtc%d1%a(i) /= achar(iachar('A')+i-1) ) error stop 58
      if ( dtc%d1%d0%a(i) /= achar(iachar('A')+i-1) ) error stop 60
   end do

   do i = 1, 3
      if ( dtc%b(i) /= iachar('A')+i-1 ) error stop 62
      if ( dtc%d1%b(i) /= iachar('A')+i-1 ) error stop 64
      if ( dtc%d1%d0%b(i) /= iachar('A')+i-1 ) error stop 66
   end do

!! Test 7

   call initdts0(dta)

   ret = fnt7(dta)

   do i = 1, 4
      if ( dta%a(i) /= achar(iachar('A')+i-1) ) error stop 68
   end do

   do i = 1, 3
      if ( dta%b(i) /= iachar('A')+i-1 ) error stop 70
   end do

!! Test 7a

   call initdts0(dta)

   ret = fnt7a(dta)

   do i = 1, 4
      if ( dta%a(i) /= achar(iachar('A')+i-1) ) error stop 72
   end do

   do i = 1, 3
      if ( dta%b(i) /= iachar('A')+i-1 ) error stop 74
   end do

!! Test 8

   call initdts0(dta)

   ret = fnt8(dta)

   do i = 1, 4
      if ( dta%a(i) /= achar(iachar('A')+i-1) ) error stop 76
   end do

   do i = 1, 3
      if ( dta%b(i) /= iachar('A')+i-1 ) error stop 78
   end do

!! Test 8a

   call initdts0(dta)

   ret = fnt8a(dta)

   do i = 1, 4
      if ( dta%a(i) /= achar(iachar('A')+i-1) ) error stop 80
   end do

   do i = 1, 3
      if ( dta%b(i) /= iachar('A')+i-1 ) error stop 82
   end do

!! Test 9

   call initdts1(dtb)

   ret = fnt9(dtb)

   do i = 1, 4
      if ( dtb%a(i) /= achar(iachar('A')+i-1) ) error stop 84
      if ( dtb%d0%a(i) /= achar(iachar('A')+i-1) ) error stop 86
   end do

   do i = 1, 3
      if ( dtb%b(i) /= iachar('A')+i-1 ) error stop 88
      if ( dtb%d0%b(i) /= iachar('A')+i-1 ) error stop 90
   end do

!! Test 9a

   call initdts1(dtb)

   ret = fnt9a(dtb)

   do i = 1, 4
      if ( dtb%a(i) /= achar(iachar('A')+i-1) ) error stop 92
      if ( dtb%d0%a(i) /= achar(iachar('A')+i-1) ) error stop 94
   end do

   do i = 1, 3
      if ( dtb%b(i) /= iachar('A')+i-1 ) error stop 96
      if ( dtb%d0%b(i) /= iachar('A')+i-1 ) error stop 98
   end do

!! Test 10

   call initdts1(dtb)

   ret = fnt10(dtb)

   do i = 1, 4
      if ( dtb%a(i) /= achar(iachar('A')+i-1) ) error stop 100
      if ( dtb%d0%a(i) /= achar(iachar('A')+i-1) ) error stop 102
   end do

   do i = 1, 3
      if ( dtb%b(i) /= iachar('A')+i-1 ) error stop 104
      if ( dtb%d0%b(i) /= iachar('A')+i-1 ) error stop 106
   end do

!! Test 10a

   call initdts1(dtb)

   ret = fnt10a(dtb)

   do i = 1, 4
      if ( dtb%a(i) /= achar(iachar('A')+i-1) ) error stop 108
      if ( dtb%d0%a(i) /= achar(iachar('A')+i-1) ) error stop 110
   end do

   do i = 1, 3
      if ( dtb%b(i) /= iachar('A')+i-1 ) error stop 112
      if ( dtb%d0%b(i) /= iachar('A')+i-1 ) error stop 114
   end do

!! Test 11

   call initdts2(dtc)

   ret = fnt11(dtc)

   do i = 1, 4
      if ( dtc%a(i) /= achar(iachar('A')+i-1) ) error stop 116
      if ( dtc%d1%a(i) /= achar(iachar('A')+i-1) ) error stop 118
      if ( dtc%d1%d0%a(i) /= achar(iachar('A')+i-1) ) error stop 120
   end do

   do i = 1, 3
      if ( dtc%b(i) /= iachar('A')+i-1 ) error stop 122
      if ( dtc%d1%b(i) /= iachar('A')+i-1 ) error stop 124
      if ( dtc%d1%d0%b(i) /= iachar('A')+i-1 ) error stop 126
   end do

!! Test 11a

   call initdts2(dtc)

   ret = fnt11a(dtc)

   do i = 1, 4
      if ( dtc%a(i) /= achar(iachar('A')+i-1) ) error stop 128
      if ( dtc%d1%a(i) /= achar(iachar('A')+i-1) ) error stop 130
      if ( dtc%d1%d0%a(i) /= achar(iachar('A')+i-1) ) error stop 132
   end do

   do i = 1, 3
      if ( dtc%b(i) /= iachar('A')+i-1 ) error stop 134
      if ( dtc%d1%b(i) /= iachar('A')+i-1 ) error stop 136
      if ( dtc%d1%d0%b(i) /= iachar('A')+i-1 ) error stop 138
   end do

!! Test 12

   call initdts2(dtc)

   ret = fnt12(dtc)

   do i = 1, 4
      if ( dtc%a(i) /= achar(iachar('A')+i-1) ) error stop 140
      if ( dtc%d1%a(i) /= achar(iachar('A')+i-1) ) error stop 142
      if ( dtc%d1%d0%a(i) /= achar(iachar('A')+i-1) ) error stop 144
   end do

   do i = 1, 3
      if ( dtc%b(i) /= iachar('A')+i-1 ) error stop 146
      if ( dtc%d1%b(i) /= iachar('A')+i-1 ) error stop 148
      if ( dtc%d1%d0%b(i) /= iachar('A')+i-1 ) error stop 150
   end do

!! Test 12a

   call initdts2(dtc)

   ret = fnt12a(dtc)

   do i = 1, 4
      if ( dtc%a(i) /= achar(iachar('A')+i-1) ) error stop 152
      if ( dtc%d1%a(i) /= achar(iachar('A')+i-1) ) error stop 154
      if ( dtc%d1%d0%a(i) /= achar(iachar('A')+i-1) ) error stop 156
   end do

   do i = 1, 3
      if ( dtc%b(i) /= iachar('A')+i-1 ) error stop 158
      if ( dtc%d1%b(i) /= iachar('A')+i-1 ) error stop 160
      if ( dtc%d1%d0%b(i) /= iachar('A')+i-1 ) error stop 162
   end do

!! Test 13

   call initdts0(dta)

   ret = fnt13(dta)

   do i = 1, 4
      if ( dta%a(i) /= achar(iachar('A')+i+3) ) error stop 164
   end do

   do i = 1, 3
      if ( dta%b(i) /= iachar('A')+i+3 ) error stop 166
   end do

!! Test 14

   call initdts1(dtb)

   ret = fnt14(dtb)

   do i = 1, 4
      if ( dtb%a(i) /= achar(iachar('A')+i+3) ) error stop 168
      if ( dtb%d0%a(i) /= achar(iachar('A')+i+3) ) error stop 170
   end do

   do i = 1, 3
      if ( dtb%b(i) /= iachar('A')+i+3 ) error stop 172
      if ( dtb%d0%b(i) /= iachar('A')+i+3 ) error stop 174
   end do

!! Test 15

   call initdts2(dtc)

   ret = fnt15(dtc)

   do i = 1, 4
      if ( dtc%a(i) /= achar(iachar('A')+i+3) ) error stop 176
      if ( dtc%d1%a(i) /= achar(iachar('A')+i+3) ) error stop 178
      if ( dtc%d1%d0%a(i) /= achar(iachar('A')+i+3) ) error stop 180
   end do

   do i = 1, 3
      if ( dtc%b(i) /= iachar('A')+i+3 ) error stop 182
      if ( dtc%d1%b(i) /= iachar('A')+i+3 ) error stop 184
      if ( dtc%d1%d0%b(i) /= iachar('A')+i+3 ) error stop 186
   end do

end program fxisok20a

subroutine initdts0(dt)
   use mxisob20a

   type(dts0) :: dt

   do i = 1, 4
      dt%a(i) = achar(iachar('A')+i-1)
   end do

   do i = 1, 3
      dt%b(i) = iachar('A')+i-1
   end do

end subroutine initdts0

subroutine initdts1(dt)
   use mxisob20a

   type(dts1) :: dt

   do i = 1, 4
      dt%a(i) = achar(iachar('A')+i-1)
   end do

   do i = 1, 3
      dt%b(i) = iachar('A')+i-1
   end do

   call initdts0(dt%d0)

end subroutine initdts1

subroutine initdts2(dt)
   use mxisob20a

   type(dts2) :: dt

   do i = 1, 4
      dt%a(i) = achar(iachar('A')+i-1)
   end do

   do i = 1, 3
      dt%b(i) = iachar('A')+i-1
   end do

   call initdts1(dt%d1)

end subroutine initdts2
