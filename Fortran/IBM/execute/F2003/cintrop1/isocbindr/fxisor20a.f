!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisor00.presh fxisor20a cxisor20a
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
!*  KEYWORD(S)                 : C_BOOL
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_BOOL
!*      - using C functions with interfaces to FORTRAN functions
!*      - passing derived types with 1-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob20a
   use ISO_C_BINDING

   type, bind(c) :: dts0
      logical(C_BOOL) :: a(5)
   end type

   type, bind(c) :: dts1
      logical(C_BOOL) :: a(5)
      type(dts0) :: d0
   end type

   type, bind(c) :: dts2
      logical(C_BOOL) :: a(5)
      type(dts1) :: d1
   end type

end module mxisob20a

program fxisor20a
   use ISO_C_BINDING
   use mxisob20a

   interface
      logical(C_BOOL) function fnt1(dt) bind(c)
         use mxisob20a
         type(dts0), intent(inout) :: dt
      end function fnt1
      logical(C_BOOL) function fnt2(dt) bind(c)
         use mxisob20a
         type(dts0), value :: dt
      end function fnt2
      logical(C_BOOL) function fnt3(dt) bind(c)
         use mxisob20a
         type(dts1), intent(inout) :: dt
      end function fnt3
      logical(C_BOOL) function fnt4(dt) bind(c)
         use mxisob20a
         type(dts1), value :: dt
      end function fnt4
      logical(C_BOOL) function fnt5(dt) bind(c)
         use mxisob20a
         type(dts2), intent(inout) :: dt
      end function fnt5
      logical(C_BOOL) function fnt6(dt) bind(c)
         use mxisob20a
         type(dts2), value :: dt
      end function fnt6
      logical(C_BOOL) function fnt7(dt) bind(c)
         use mxisob20a
         type(dts0), intent(in) :: dt
      end function fnt7
      logical(C_BOOL) function fnt7a(dt) bind(c)
         use mxisob20a
         type(dts0), intent(in) :: dt
      end function fnt7a
      logical(C_BOOL) function fnt8(dt) bind(c)
         use mxisob20a
         type(dts0), intent(in), value :: dt
      end function fnt8
      logical(C_BOOL) function fnt8a(dt) bind(c)
         use mxisob20a
         type(dts0), intent(in), value :: dt
      end function fnt8a
      logical(C_BOOL) function fnt9(dt) bind(c)
         use mxisob20a
         type(dts1), intent(in) :: dt
      end function fnt9
      logical(C_BOOL) function fnt9a(dt) bind(c)
         use mxisob20a
         type(dts1), intent(in) :: dt
      end function fnt9a
      logical(C_BOOL) function fnt10(dt) bind(c)
         use mxisob20a
         type(dts1), intent(in), value :: dt
      end function fnt10
      logical(C_BOOL) function fnt10a(dt) bind(c)
         use mxisob20a
         type(dts1), intent(in), value :: dt
      end function fnt10a
      logical(C_BOOL) function fnt11(dt) bind(c)
         use mxisob20a
         type(dts2), intent(in) :: dt
      end function fnt11
      logical(C_BOOL) function fnt11a(dt) bind(c)
         use mxisob20a
         type(dts2), intent(in) :: dt
      end function fnt11a
      logical(C_BOOL) function fnt12(dt) bind(c)
         use mxisob20a
         type(dts2), intent(in), value :: dt
      end function fnt12
      logical(C_BOOL) function fnt12a(dt) bind(c)
         use mxisob20a
         type(dts2), intent(in), value :: dt
      end function fnt12a
      logical(C_BOOL) function fnt13(dt) bind(c)
         use mxisob20a
         type(dts0), intent(out) :: dt
      end function fnt13
      logical(C_BOOL) function fnt14(dt) bind(c)
         use mxisob20a
         type(dts1), intent(out) :: dt
      end function fnt14
      logical(C_BOOL) function fnt15(dt) bind(c)
         use mxisob20a
         type(dts2), intent(out) :: dt
      end function fnt15
   end interface

   type(dts0) :: dta
   type(dts1) :: dtb
   type(dts2) :: dtc
   logical ret

!! Test 1

   call initdts0(dta)

   ret = fnt1(dta)

   do i = 1, 5
      if ( dta%a(i) .eqv. .true. ) error stop 20
   end do


!! Test 2

   call initdts0(dta)

   ret = fnt2(dta)

   do i = 1, 5
      if ( dta%a(i) .neqv. .true. ) error stop 22
   end do


!! Test 3

   call initdts1(dtb)

   ret = fnt3(dtb)

   do i = 1, 5
      if ( dtb%a(i) .eqv. .true. ) error stop 24
      if ( dtb%d0%a(i) .eqv. .true. ) error stop 26
   end do


!! Test 4

   call initdts1(dtb)

   ret = fnt4(dtb)

   do i = 1, 5
      if ( dtb%a(i) .neqv. .true. ) error stop 28
      if ( dtb%d0%a(i) .neqv. .true. ) error stop 30
   end do


!! Test 5

   call initdts2(dtc)

   ret = fnt5(dtc)

   do i = 1, 5
      if ( dtc%a(i) .eqv. .true. ) error stop 32
      if ( dtc%d1%a(i) .eqv. .true. ) error stop 34
      if ( dtc%d1%d0%a(i) .eqv. .true. ) error stop 36
   end do


!! Test 6

   call initdts2(dtc)

   ret = fnt6(dtc)

   do i = 1, 5
      if ( dtc%a(i) .neqv. .true. ) error stop 38
      if ( dtc%d1%a(i) .neqv. .true. ) error stop 40
      if ( dtc%d1%d0%a(i) .neqv. .true. ) error stop 42
   end do


!! Test 7

   call initdts0(dta)

   ret = fnt7(dta)

   do i = 1, 5
      if ( dta%a(i) .neqv. .true. ) error stop 44
   end do


!! Test 7a

   call initdts0(dta)

   ret = fnt7a(dta)

   do i = 1, 5
      if ( dta%a(i) .neqv. .true. ) error stop 46
   end do


!! Test 8

   call initdts0(dta)

   ret = fnt8(dta)

   do i = 1, 5
      if ( dta%a(i) .neqv. .true. ) error stop 48
   end do


!! Test 8a

   call initdts0(dta)

   ret = fnt8a(dta)

   do i = 1, 5
      if ( dta%a(i) .neqv. .true. ) error stop 50
   end do


!! Test 9

   call initdts1(dtb)

   ret = fnt9(dtb)

   do i = 1, 5
      if ( dtb%a(i) .neqv. .true. ) error stop 52
      if ( dtb%d0%a(i) .neqv. .true. ) error stop 54
   end do


!! Test 9a

   call initdts1(dtb)

   ret = fnt9a(dtb)

   do i = 1, 5
      if ( dtb%a(i) .neqv. .true. ) error stop 56
      if ( dtb%d0%a(i) .neqv. .true. ) error stop 58
   end do


!! Test 10

   call initdts1(dtb)

   ret = fnt10(dtb)

   do i = 1, 5
      if ( dtb%a(i) .neqv. .true. ) error stop 60
      if ( dtb%d0%a(i) .neqv. .true. ) error stop 62
   end do


!! Test 10a

   call initdts1(dtb)

   ret = fnt10a(dtb)

   do i = 1, 5
      if ( dtb%a(i) .neqv. .true. ) error stop 64
      if ( dtb%d0%a(i) .neqv. .true. ) error stop 66
   end do


!! Test 11

   call initdts2(dtc)

   ret = fnt11(dtc)

   do i = 1, 5
      if ( dtc%a(i) .neqv. .true. ) error stop 68
      if ( dtc%d1%a(i) .neqv. .true. ) error stop 70
      if ( dtc%d1%d0%a(i) .neqv. .true. ) error stop 72
   end do


!! Test 11a

   call initdts2(dtc)

   ret = fnt11a(dtc)

   do i = 1, 5
      if ( dtc%a(i) .neqv. .true. ) error stop 74
      if ( dtc%d1%a(i) .neqv. .true. ) error stop 76
      if ( dtc%d1%d0%a(i) .neqv. .true. ) error stop 78
   end do


!! Test 12

   call initdts2(dtc)

   ret = fnt12(dtc)

   do i = 1, 5
      if ( dtc%a(i) .neqv. .true. ) error stop 80
      if ( dtc%d1%a(i) .neqv. .true. ) error stop 82
      if ( dtc%d1%d0%a(i) .neqv. .true. ) error stop 84
   end do


!! Test 12a

   call initdts2(dtc)

   ret = fnt12a(dtc)

   do i = 1, 5
      if ( dtc%a(i) .neqv. .true. ) error stop 86
      if ( dtc%d1%a(i) .neqv. .true. ) error stop 88
      if ( dtc%d1%d0%a(i) .neqv. .true. ) error stop 90
   end do


!! Test 13

   call initdts0(dta)

   ret = fnt13(dta)

   do i = 1, 5
      if ( dta%a(i) .eqv. .true. ) error stop 92
   end do


!! Test 14

   call initdts1(dtb)

   ret = fnt14(dtb)

   do i = 1, 5
      if ( dtb%a(i) .eqv. .true. ) error stop 94
      if ( dtb%d0%a(i) .eqv. .true. ) error stop 96
   end do


!! Test 15

   call initdts2(dtc)

   ret = fnt15(dtc)

   do i = 1, 5
      if ( dtc%a(i) .eqv. .true. ) error stop 98
      if ( dtc%d1%a(i) .eqv. .true. ) error stop 100
      if ( dtc%d1%d0%a(i) .eqv. .true. ) error stop 102
   end do


end program fxisor20a

subroutine initdts0(dt)
   use mxisob20a

   type(dts0) :: dt

   do i = 1, 5
      dt%a(i) = .true.
   end do


end subroutine initdts0

subroutine initdts1(dt)
   use mxisob20a

   type(dts1) :: dt

   do i = 1, 5
      dt%a(i) = .true.
   end do


   call initdts0(dt%d0)

end subroutine initdts1

subroutine initdts2(dt)
   use mxisob20a

   type(dts2) :: dt

   do i = 1, 5
      dt%a(i) = .true.
   end do


   call initdts1(dt%d1)

end subroutine initdts2
