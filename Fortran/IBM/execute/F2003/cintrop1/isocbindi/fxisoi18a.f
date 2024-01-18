!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoi00.presh fxisoi18a cxisoi18a
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
!*  KEYWORD(S)                 : C_INT_FAST16_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*
!*	- testing C_INT_FAST16_T
!*	- using external FORTRAN functions
!*	- passing derived types with 1-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob18a
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

end module mxisob18a

integer(C_INT_FAST16_T) function fnt1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts0), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 20
      dt%a(i) = i+1
   end do


   fnt1 = 0
end function fnt1

integer(C_INT_FAST16_T) function fnt2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts0), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 22
      dt%a(i) = i+1
   end do


   fnt2 = 0
end function fnt2

integer(C_INT_FAST16_T) function fnt3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts1), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 24
      dt%a(i) = dt%a(i) + i
      if ( dt%d0%a(i) /= i ) error stop 26
      dt%d0%a(i) = dt%d0%a(i) + i
   end do


   fnt3 = 0
end function fnt3

integer(C_INT_FAST16_T) function fnt4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts1), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 28
      dt%a(i) = dt%a(i) + i
      if ( dt%d0%a(i) /= i ) error stop 30
      dt%d0%a(i) = dt%d0%a(i) + i
   end do


   fnt4 = 0
end function fnt4

integer(C_INT_FAST16_T) function fnt5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts2), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 32
      dt%a(i) = dt%a(i) + i
      if ( dt%d1%a(i) /= i ) error stop 34
      dt%d1%a(i) = dt%d1%a(i) + i
      if ( dt%d1%d0%a(i) /= i ) error stop 36
      dt%d1%d0%a(i) = dt%d1%d0%a(i) + i
   end do


   fnt5 = 0
end function fnt5

integer(C_INT_FAST16_T) function fnt6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts2), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 38
      dt%a(i) = dt%a(i) + i
      if ( dt%d1%a(i) /= i ) error stop 40
      dt%d1%a(i) = dt%d1%a(i) + i
      if ( dt%d1%d0%a(i) /= i ) error stop 42
      dt%d1%d0%a(i) = dt%d1%d0%a(i) + i
   end do


   fnt6 = 0
end function fnt6

integer(C_INT_FAST16_T) function fnt7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts0), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 44
   end do


   fnt7 = 0
end function fnt7

integer(C_INT_FAST16_T) function fnt7a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts0), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 46
   end do


   fnt7a = 0
end function fnt7a

integer(C_INT_FAST16_T) function fnt8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts0), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 48
   end do


   fnt8 = 0
end function fnt8

integer(C_INT_FAST16_T) function fnt8a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts0), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 50
   end do


   fnt8a = 0
end function fnt8a

integer(C_INT_FAST16_T) function fnt9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts1), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 52
      if ( dt%d0%a(i) /= i ) error stop 54
   end do


   fnt9 = 0
end function fnt9

integer(C_INT_FAST16_T) function fnt9a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts1), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 56
      if ( dt%d0%a(i) /= i ) error stop 58
   end do


   fnt9a = 0
end function fnt9a

integer(C_INT_FAST16_T) function fnt10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts1), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 60
      if ( dt%d0%a(i) /= i ) error stop 62
   end do


   fnt10 = 0
end function fnt10

integer(C_INT_FAST16_T) function fnt10a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts1), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 64
      if ( dt%d0%a(i) /= i ) error stop 66
   end do


   fnt10a = 0
end function fnt10a

integer(C_INT_FAST16_T) function fnt11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts2), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 68
      if ( dt%d1%a(i) /= i ) error stop 70
      if ( dt%d1%d0%a(i) /= i ) error stop 72
   end do


   fnt11 = 0
end function fnt11

integer(C_INT_FAST16_T) function fnt11a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts2), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 74
      if ( dt%d1%a(i) /= i ) error stop 76
      if ( dt%d1%d0%a(i) /= i ) error stop 78
   end do


   fnt11a = 0
end function fnt11a

integer(C_INT_FAST16_T) function fnt12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts2), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 80
      if ( dt%d1%a(i) /= i ) error stop 82
      if ( dt%d1%d0%a(i) /= i ) error stop 84
   end do


   fnt12 = 0
end function fnt12

integer(C_INT_FAST16_T) function fnt12a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts2), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 86
      if ( dt%d1%a(i) /= i ) error stop 88
      if ( dt%d1%d0%a(i) /= i ) error stop 90
   end do


   fnt12a = 0
end function fnt12a

integer(C_INT_FAST16_T) function fnt13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts0), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = i+1
   end do


   fnt13 = 0
end function fnt13

integer(C_INT_FAST16_T) function fnt14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts1), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = dt%a(i) + i
      dt%d0%a(i) = dt%d0%a(i) + i
   end do


   fnt14 = 0
end function fnt14

integer(C_INT_FAST16_T) function fnt15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts2), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = dt%a(i) + i
      dt%d1%a(i) = dt%d1%a(i) + i
      dt%d1%d0%a(i) = dt%d1%d0%a(i) + i
   end do


   fnt15 = 0
end function fnt15
