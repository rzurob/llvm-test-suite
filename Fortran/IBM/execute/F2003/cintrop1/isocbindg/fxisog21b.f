!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisog21b cxisog20b
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
!*  KEYWORD(S)                 : C_INT_FAST8_T, C_INTMAX_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_INT_FAST8_T and C_INTMAX_T
!*      - using C functions with interfaces to FORTRAN functions
!*      - function interfaces defined in module
!*      - passing derived types with 2-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob21b
   use ISO_C_BINDING

   type, bind(c) :: dtd0
      integer(C_INT_FAST8_T) :: a(10,5)
      integer(C_INTMAX_T) :: b(6,3)
   end type

   type, bind(c) :: dtd1
      integer(C_INT_FAST8_T) :: a(10,5)
      integer(C_INTMAX_T) :: b(6,3)
      type(dtd0) :: d0
   end type

   type, bind(c) :: dtd2
      integer(C_INT_FAST8_T) :: a(10,5)
      integer(C_INTMAX_T) :: b(6,3)
      type(dtd1) :: d1
   end type

end module mxisob21b

module mxisob21c
   interface
      integer(C_INT_FAST8_T) function fnt1(dt) bind(c)
         use mxisob21b
         type(dtd0), intent(inout) :: dt
      end function fnt1
      integer(C_INT_FAST8_T) function fnt2(dt) bind(c)
         use mxisob21b
         type(dtd0), value :: dt
      end function fnt2
      integer(C_INT_FAST8_T) function fnt3(dt) bind(c)
         use mxisob21b
         type(dtd1), intent(inout) :: dt
      end function fnt3
      integer(C_INT_FAST8_T) function fnt4(dt) bind(c)
         use mxisob21b
         type(dtd1), value :: dt
      end function fnt4
      integer(C_INT_FAST8_T) function fnt5(dt) bind(c)
         use mxisob21b
         type(dtd2), intent(inout) :: dt
      end function fnt5
      integer(C_INT_FAST8_T) function fnt6(dt) bind(c)
         use mxisob21b
         type(dtd2), value :: dt
      end function fnt6
      integer(C_INT_FAST8_T) function fnt7(dt) bind(c)
         use mxisob21b
         type(dtd0), intent(in) :: dt
      end function fnt7
      integer(C_INT_FAST8_T) function fnt7a(dt) bind(c)
         use mxisob21b
         type(dtd0), intent(in) :: dt
      end function fnt7a
      integer(C_INT_FAST8_T) function fnt8(dt) bind(c)
         use mxisob21b
         type(dtd0), intent(in), value :: dt
      end function fnt8
      integer(C_INT_FAST8_T) function fnt8a(dt) bind(c)
         use mxisob21b
         type(dtd0), intent(in), value :: dt
      end function fnt8a
      integer(C_INT_FAST8_T) function fnt9(dt) bind(c)
         use mxisob21b
         type(dtd1), intent(in) :: dt
      end function fnt9
      integer(C_INT_FAST8_T) function fnt9a(dt) bind(c)
         use mxisob21b
         type(dtd1), intent(in) :: dt
      end function fnt9a
      integer(C_INT_FAST8_T) function fnt10(dt) bind(c)
         use mxisob21b
         type(dtd1), intent(in), value :: dt
      end function fnt10
      integer(C_INT_FAST8_T) function fnt10a(dt) bind(c)
         use mxisob21b
         type(dtd1), intent(in), value :: dt
      end function fnt10a
      integer(C_INT_FAST8_T) function fnt11(dt) bind(c)
         use mxisob21b
         type(dtd2), intent(in) :: dt
      end function fnt11
      integer(C_INT_FAST8_T) function fnt11a(dt) bind(c)
         use mxisob21b
         type(dtd2), intent(in) :: dt
      end function fnt11a
      integer(C_INT_FAST8_T) function fnt12(dt) bind(c)
         use mxisob21b
         type(dtd2), intent(in), value :: dt
      end function fnt12
      integer(C_INT_FAST8_T) function fnt12a(dt) bind(c)
         use mxisob21b
         type(dtd2), intent(in), value :: dt
      end function fnt12a
      integer(C_INT_FAST8_T) function fnt13(dt) bind(c)
         use mxisob21b
         type(dtd0), intent(out) :: dt
      end function fnt13
      integer(C_INT_FAST8_T) function fnt14(dt) bind(c)
         use mxisob21b
         type(dtd1), intent(out) :: dt
      end function fnt14
      integer(C_INT_FAST8_T) function fnt15(dt) bind(c)
         use mxisob21b
         type(dtd2), intent(out) :: dt
      end function fnt15
   end interface
end module mxisob21c

program fxisog21b
   use ISO_C_BINDING
   use mxisob21b
   use mxisob21c

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

   do i = 1, 3
      do j = 1, 6
         if ( dta%b(j,i) /= i+j ) error stop 22
      end do
   end do

!! Test 2

   call initdtd0(dta)

   ret = fnt2(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= i+j-1 ) error stop 24
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dta%b(j,i) /= i+j-1 ) error stop 26
      end do
   end do

!! Test 3

   call initdtd1(dtb)

   ret = fnt3(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j ) error stop 28
         if ( dtb%d0%a(j,i) /= i+j ) error stop 30
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtb%b(j,i) /= i+j ) error stop 32
         if ( dtb%d0%b(j,i) /= i+j ) error stop 34
      end do
   end do

!! Test 4

   call initdtd1(dtb)

   ret = fnt4(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j-1 ) error stop 36
         if ( dtb%d0%a(j,i) /= i+j-1 ) error stop 38
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtb%b(j,i) /= i+j-1 ) error stop 40
         if ( dtb%d0%b(j,i) /= i+j-1 ) error stop 42
      end do
   end do

!! Test 5

   call initdtd2(dtc)

   ret = fnt5(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j ) error stop 44
         if ( dtc%d1%a(j,i) /= i+j ) error stop 46
         if ( dtc%d1%d0%a(j,i) /= i+j ) error stop 48
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtc%b(j,i) /= i+j ) error stop 50
         if ( dtc%d1%b(j,i) /= i+j ) error stop 52
         if ( dtc%d1%d0%b(j,i) /= i+j ) error stop 54
      end do
   end do

!! Test 6

   call initdtd2(dtc)

   ret = fnt6(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j-1 ) error stop 56
         if ( dtc%d1%a(j,i) /= i+j-1 ) error stop 58
         if ( dtc%d1%d0%a(j,i) /= i+j-1 ) error stop 60
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtc%b(j,i) /= i+j-1 ) error stop 62
         if ( dtc%d1%b(j,i) /= i+j-1 ) error stop 64
         if ( dtc%d1%d0%b(j,i) /= i+j-1 ) error stop 66
      end do
   end do

!! Test 7

   call initdtd0(dta)

   ret = fnt7(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= i+j-1 ) error stop 68
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dta%b(j,i) /= i+j-1 ) error stop 70
      end do
   end do

!! Test 7a

   call initdtd0(dta)

   ret = fnt7a(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= i+j-1 ) error stop 72
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dta%b(j,i) /= i+j-1 ) error stop 74
      end do
   end do

!! Test 8

   call initdtd0(dta)

   ret = fnt8(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= i+j-1 ) error stop 76
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dta%b(j,i) /= i+j-1 ) error stop 78
      end do
   end do

!! Test 8a

   call initdtd0(dta)

   ret = fnt8a(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= i+j-1 ) error stop 80
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dta%b(j,i) /= i+j-1 ) error stop 82
      end do
   end do

!! Test 9

   call initdtd1(dtb)

   ret = fnt9(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j-1 ) error stop 84
         if ( dtb%d0%a(j,i) /= i+j-1 ) error stop 86
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtb%b(j,i) /= i+j-1 ) error stop 88
         if ( dtb%d0%b(j,i) /= i+j-1 ) error stop 90
      end do
   end do

!! Test 9a

   call initdtd1(dtb)

   ret = fnt9a(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j-1 ) error stop 92
         if ( dtb%d0%a(j,i) /= i+j-1 ) error stop 94
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtb%b(j,i) /= i+j-1 ) error stop 96
         if ( dtb%d0%b(j,i) /= i+j-1 ) error stop 98
      end do
   end do

!! Test 10

   call initdtd1(dtb)

   ret = fnt10(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j-1 ) error stop 100
         if ( dtb%d0%a(j,i) /= i+j-1 ) error stop 102
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtb%b(j,i) /= i+j-1 ) error stop 104
         if ( dtb%d0%b(j,i) /= i+j-1 ) error stop 106
      end do
   end do

!! Test 10a

   call initdtd1(dtb)

   ret = fnt10a(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j-1 ) error stop 108
         if ( dtb%d0%a(j,i) /= i+j-1 ) error stop 110
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtb%b(j,i) /= i+j-1 ) error stop 112
         if ( dtb%d0%b(j,i) /= i+j-1 ) error stop 114
      end do
   end do

!! Test 11

   call initdtd2(dtc)

   ret = fnt11(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j-1 ) error stop 116
         if ( dtc%d1%a(j,i) /= i+j-1 ) error stop 118
         if ( dtc%d1%d0%a(j,i) /= i+j-1 ) error stop 120
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtc%b(j,i) /= i+j-1 ) error stop 122
         if ( dtc%d1%b(j,i) /= i+j-1 ) error stop 124
         if ( dtc%d1%d0%b(j,i) /= i+j-1 ) error stop 126
      end do
   end do

!! Test 11a

   call initdtd2(dtc)

   ret = fnt11a(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j-1 ) error stop 128
         if ( dtc%d1%a(j,i) /= i+j-1 ) error stop 130
         if ( dtc%d1%d0%a(j,i) /= i+j-1 ) error stop 132
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtc%b(j,i) /= i+j-1 ) error stop 134
         if ( dtc%d1%b(j,i) /= i+j-1 ) error stop 136
         if ( dtc%d1%d0%b(j,i) /= i+j-1 ) error stop 138
      end do
   end do

!! Test 12

   call initdtd2(dtc)

   ret = fnt12(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j-1 ) error stop 140
         if ( dtc%d1%a(j,i) /= i+j-1 ) error stop 142
         if ( dtc%d1%d0%a(j,i) /= i+j-1 ) error stop 144
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtc%b(j,i) /= i+j-1 ) error stop 146
         if ( dtc%d1%b(j,i) /= i+j-1 ) error stop 148
         if ( dtc%d1%d0%b(j,i) /= i+j-1 ) error stop 150
      end do
   end do

!! Test 12a

   call initdtd2(dtc)

   ret = fnt12a(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j-1 ) error stop 152
         if ( dtc%d1%a(j,i) /= i+j-1 ) error stop 154
         if ( dtc%d1%d0%a(j,i) /= i+j-1 ) error stop 156
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtc%b(j,i) /= i+j-1 ) error stop 158
         if ( dtc%d1%b(j,i) /= i+j-1 ) error stop 160
         if ( dtc%d1%d0%b(j,i) /= i+j-1 ) error stop 162
      end do
   end do

!! Test 13

   call initdtd0(dta)

   ret = fnt13(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) /= i+j ) error stop 164
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dta%b(j,i) /= i+j ) error stop 166
      end do
   end do

!! Test 14

   call initdtd1(dtb)

   ret = fnt14(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) /= i+j ) error stop 168
         if ( dtb%d0%a(j,i) /= i+j ) error stop 170
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtb%b(j,i) /= i+j ) error stop 172
         if ( dtb%d0%b(j,i) /= i+j ) error stop 174
      end do
   end do

!! Test 15

   call initdtd2(dtc)

   ret = fnt15(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) /= i+j ) error stop 176
         if ( dtc%d1%a(j,i) /= i+j ) error stop 178
         if ( dtc%d1%d0%a(j,i) /= i+j ) error stop 180
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dtc%b(j,i) /= i+j ) error stop 182
         if ( dtc%d1%b(j,i) /= i+j ) error stop 184
         if ( dtc%d1%d0%b(j,i) /= i+j ) error stop 186
      end do
   end do

end program fxisog21b

subroutine initdtd0(dt)
   use mxisob21b

   type(dtd0) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = i+j-1
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         dt%b(j,i) = i+j-1
      end do
   end do

end subroutine initdtd0

subroutine initdtd1(dt)
   use mxisob21b

   type(dtd1) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = i+j-1
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         dt%b(j,i) = i+j-1
      end do
   end do

   call initdtd0(dt%d0)

end subroutine initdtd1

subroutine initdtd2(dt)
   use mxisob21b

   type(dtd2) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = i+j-1
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         dt%b(j,i) = i+j-1
      end do
   end do

   call initdtd1(dt%d1)

end subroutine initdtd2
