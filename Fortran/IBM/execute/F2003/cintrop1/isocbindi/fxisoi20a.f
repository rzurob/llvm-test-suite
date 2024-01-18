!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoi00.presh fxisoi20a cxisoi20a
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 4/23/2002
!*  ORIGIN                     : AIX Compiler Development,
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
!*      - using C functions with interfaces to FORTRAN functions
!*      - passing derived types with 1-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob20a
   use ISO_C_BINDING

   type, bind(c) :: dts0
      integer(C_INT_FAST16_T) :: a(5)
   end type

   type, bind(c) :: dts1
      integer(C_INT_FAST16_T) :: a(5)
      type(dts0) :: d0
   end type

   type, bind(c) :: dts2
      integer(C_INT_FAST16_T) :: a(5)
      type(dts1) :: d1
   end type

end module mxisob20a

program fxisoi20a
   use ISO_C_BINDING
   use mxisob20a

   interface
      integer(C_INT_FAST16_T) function fnt1(dt) bind(c)
         use mxisob20a
         type(dts0), intent(inout) :: dt
      end function fnt1
      integer(C_INT_FAST16_T) function fnt2(dt) bind(c)
         use mxisob20a
         type(dts0), value :: dt
      end function fnt2
      integer(C_INT_FAST16_T) function fnt3(dt) bind(c)
         use mxisob20a
         type(dts1), intent(inout) :: dt
      end function fnt3
      integer(C_INT_FAST16_T) function fnt4(dt) bind(c)
         use mxisob20a
         type(dts1), value :: dt
      end function fnt4
      integer(C_INT_FAST16_T) function fnt5(dt) bind(c)
         use mxisob20a
         type(dts2), intent(inout) :: dt
      end function fnt5
      integer(C_INT_FAST16_T) function fnt6(dt) bind(c)
         use mxisob20a
         type(dts2), value :: dt
      end function fnt6
      integer(C_INT_FAST16_T) function fnt7(dt) bind(c)
         use mxisob20a
         type(dts0), intent(in) :: dt
      end function fnt7
      integer(C_INT_FAST16_T) function fnt7a(dt) bind(c)
         use mxisob20a
         type(dts0), intent(in) :: dt
      end function fnt7a
      integer(C_INT_FAST16_T) function fnt8(dt) bind(c)
         use mxisob20a
         type(dts0), intent(in), value :: dt
      end function fnt8
      integer(C_INT_FAST16_T) function fnt8a(dt) bind(c)
         use mxisob20a
         type(dts0), intent(in), value :: dt
      end function fnt8a
      integer(C_INT_FAST16_T) function fnt9(dt) bind(c)
         use mxisob20a
         type(dts1), intent(in) :: dt
      end function fnt9
      integer(C_INT_FAST16_T) function fnt9a(dt) bind(c)
         use mxisob20a
         type(dts1), intent(in) :: dt
      end function fnt9a
      integer(C_INT_FAST16_T) function fnt10(dt) bind(c)
         use mxisob20a
         type(dts1), intent(in), value :: dt
      end function fnt10
      integer(C_INT_FAST16_T) function fnt10a(dt) bind(c)
         use mxisob20a
         type(dts1), intent(in), value :: dt
      end function fnt10a
      integer(C_INT_FAST16_T) function fnt11(dt) bind(c)
         use mxisob20a
         type(dts2), intent(in) :: dt
      end function fnt11
      integer(C_INT_FAST16_T) function fnt11a(dt) bind(c)
         use mxisob20a
         type(dts2), intent(in) :: dt
      end function fnt11a
      integer(C_INT_FAST16_T) function fnt12(dt) bind(c)
         use mxisob20a
         type(dts2), intent(in), value :: dt
      end function fnt12
      integer(C_INT_FAST16_T) function fnt12a(dt) bind(c)
         use mxisob20a
         type(dts2), intent(in), value :: dt
      end function fnt12a
      integer(C_INT_FAST16_T) function fnt13(dt) bind(c)
         use mxisob20a
         type(dts0), intent(out) :: dt
      end function fnt13
      integer(C_INT_FAST16_T) function fnt14(dt) bind(c)
         use mxisob20a
         type(dts1), intent(out) :: dt
      end function fnt14
      integer(C_INT_FAST16_T) function fnt15(dt) bind(c)
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

   do i = 1, 5
      if ( dta%a(i) /= i+1 ) error stop 20
   end do


!! Test 2

   call initdts0(dta)

   ret = fnt2(dta)

   do i = 1, 5
      if ( dta%a(i) /= i ) error stop 22
   end do


!! Test 3

   call initdts1(dtb)

   ret = fnt3(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= i*2 ) error stop 24
      if ( dtb%d0%a(i) /= i*2 ) error stop 26
   end do


!! Test 4

   call initdts1(dtb)

   ret = fnt4(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= i ) error stop 28
      if ( dtb%d0%a(i) /= i ) error stop 30
   end do


!! Test 5

   call initdts2(dtc)

   ret = fnt5(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= i*2 ) error stop 32
      if ( dtc%d1%a(i) /= i*2 ) error stop 34
      if ( dtc%d1%d0%a(i) /= i*2 ) error stop 36
   end do


!! Test 6

   call initdts2(dtc)

   ret = fnt6(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= i ) error stop 38
      if ( dtc%d1%a(i) /= i ) error stop 40
      if ( dtc%d1%d0%a(i) /= i ) error stop 42
   end do


!! Test 7

   call initdts0(dta)

   ret = fnt7(dta)

   do i = 1, 5
      if ( dta%a(i) /= i ) error stop 44
   end do


!! Test 7a

   call initdts0(dta)

   ret = fnt7a(dta)

   do i = 1, 5
      if ( dta%a(i) /= i ) error stop 46
   end do


!! Test 8

   call initdts0(dta)

   ret = fnt8(dta)

   do i = 1, 5
      if ( dta%a(i) /= i ) error stop 48
   end do


!! Test 8a

   call initdts0(dta)

   ret = fnt8a(dta)

   do i = 1, 5
      if ( dta%a(i) /= i ) error stop 50
   end do


!! Test 9

   call initdts1(dtb)

   ret = fnt9(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= i ) error stop 52
      if ( dtb%d0%a(i) /= i ) error stop 54
   end do


!! Test 9a

   call initdts1(dtb)

   ret = fnt9a(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= i ) error stop 56
      if ( dtb%d0%a(i) /= i ) error stop 58
   end do


!! Test 10

   call initdts1(dtb)

   ret = fnt10(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= i ) error stop 60
      if ( dtb%d0%a(i) /= i ) error stop 62
   end do


!! Test 10a

   call initdts1(dtb)

   ret = fnt10a(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= i ) error stop 64
      if ( dtb%d0%a(i) /= i ) error stop 66
   end do


!! Test 11

   call initdts2(dtc)

   ret = fnt11(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= i ) error stop 68
      if ( dtc%d1%a(i) /= i ) error stop 70
      if ( dtc%d1%d0%a(i) /= i ) error stop 72
   end do


!! Test 11a

   call initdts2(dtc)

   ret = fnt11a(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= i ) error stop 74
      if ( dtc%d1%a(i) /= i ) error stop 76
      if ( dtc%d1%d0%a(i) /= i ) error stop 78
   end do


!! Test 12

   call initdts2(dtc)

   ret = fnt12(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= i ) error stop 80
      if ( dtc%d1%a(i) /= i ) error stop 82
      if ( dtc%d1%d0%a(i) /= i ) error stop 84
   end do


!! Test 12a

   call initdts2(dtc)

   ret = fnt12a(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= i ) error stop 86
      if ( dtc%d1%a(i) /= i ) error stop 88
      if ( dtc%d1%d0%a(i) /= i ) error stop 90
   end do


!! Test 13

   call initdts0(dta)

   ret = fnt13(dta)

   do i = 1, 5
      if ( dta%a(i) /= i*2 ) error stop 92
   end do


!! Test 14

   call initdts1(dtb)

   ret = fnt14(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= i*2 ) error stop 94
      if ( dtb%d0%a(i) /= i*2 ) error stop 96
   end do


!! Test 15

   call initdts2(dtc)

   ret = fnt15(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= i*2 ) error stop 98
      if ( dtc%d1%a(i) /= i*2 ) error stop 100
      if ( dtc%d1%d0%a(i) /= i*2 ) error stop 102
   end do


end program fxisoi20a

subroutine initdts0(dt)
   use mxisob20a

   type(dts0) :: dt

   do i = 1, 5
      dt%a(i) = i
   end do


end subroutine initdts0

subroutine initdts1(dt)
   use mxisob20a

   type(dts1) :: dt

   do i = 1, 5
      dt%a(i) = i
   end do


   call initdts0(dt%d0)

end subroutine initdts1

subroutine initdts2(dt)
   use mxisob20a

   type(dts2) :: dt

   do i = 1, 5
      dt%a(i) = i
   end do


   call initdts1(dt%d1)

end subroutine initdts2
