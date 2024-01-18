!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa01.presh fxisoi26
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
!*      - testing C_INT_FAST16_T
!*      - FORTRAN code only
!*      - passing derived types with scalar fields as arguments
!*      - testing INTENT and VALUE attributes
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob26
   use ISO_C_BINDING

   type, bind(c) :: dt0
      integer(C_INT_FAST16_T) :: a
   end type

   type, bind(c) :: dt1
      integer(C_INT_FAST16_T) :: a
      type(dt0) :: d0
   end type

   type, bind(c) :: dt2
      integer(C_INT_FAST16_T) :: a
      type(dt1) :: d1
   end type

end module mxisob26

program fxisoi26
   use ISO_C_BINDING
   use mxisob26

   interface
      integer(C_INT_FAST16_T) function fnt1(dt) bind(c)
         use mxisob26
         type(dt0), intent(inout) :: dt
      end function fnt1
      integer(C_INT_FAST16_T) function fnt2(dt) bind(c)
         use mxisob26
         type(dt0), value :: dt
      end function fnt2
      integer(C_INT_FAST16_T) function fnt3(dt) bind(c)
         use mxisob26
         type(dt1), intent(inout) :: dt
      end function fnt3
      integer(C_INT_FAST16_T) function fnt4(dt) bind(c)
         use mxisob26
         type(dt1), value :: dt
      end function fnt4
      integer(C_INT_FAST16_T) function fnt5(dt) bind(c)
         use mxisob26
         type(dt2), intent(inout) :: dt
      end function fnt5
      integer(C_INT_FAST16_T) function fnt6(dt) bind(c)
         use mxisob26
         type(dt2), value :: dt
      end function fnt6
      integer(C_INT_FAST16_T) function fnt7(dt) bind(c)
         use mxisob26
         type(dt0), intent(in) :: dt
      end function fnt7
      integer(C_INT_FAST16_T) function fnt8(dt) bind(c)
         use mxisob26
         type(dt0), intent(in), value :: dt
      end function fnt8
      integer(C_INT_FAST16_T) function fnt9(dt) bind(c)
         use mxisob26
         type(dt1), intent(in) :: dt
      end function fnt9
      integer(C_INT_FAST16_T) function fnt10(dt) bind(c)
         use mxisob26
         type(dt1), intent(in), value :: dt
      end function fnt10
      integer(C_INT_FAST16_T) function fnt11(dt) bind(c)
         use mxisob26
         type(dt2), intent(in) :: dt
      end function fnt11
      integer(C_INT_FAST16_T) function fnt12(dt) bind(c)
         use mxisob26
         type(dt2), intent(in), value :: dt
      end function fnt12
      integer(C_INT_FAST16_T) function fnt13(dt) bind(c)
         use mxisob26
         type(dt0), intent(out) :: dt
      end function fnt13
      integer(C_INT_FAST16_T) function fnt14(dt) bind(c)
         use mxisob26
         type(dt1), intent(out) :: dt
      end function fnt14
      integer(C_INT_FAST16_T) function fnt15(dt) bind(c)
         use mxisob26
         type(dt2), intent(out) :: dt
      end function fnt15
   end interface

   type(dt0) :: dta
   type(dt1) :: dtb
   type(dt2) :: dtc
   integer ret

!! Test 1

   dta%a = 5

   ret = fnt1(dta)

   if ( dta%a /= 10 ) error stop 20

!! Test 2

   dta%a = 5

   ret = fnt2(dta)

   if ( dta%a /= 5 ) error stop 22

!! Test 3

   dtb%a = 5
   dtb%d0%a = 5

   ret = fnt3(dtb)

   if ( dtb%a /= 10 ) error stop 24
   if ( dtb%d0%a /= 10 ) error stop 26

!! Test 4

   dtb%a = 5
   dtb%d0%a = 5

   ret = fnt4(dtb)

   if ( dtb%a /= 5 ) error stop 28
   if ( dtb%d0%a /= 5 ) error stop 30

!! Test 5

   dtc%a = 5
   dtc%d1%a = 5
   dtc%d1%d0%a = 5

   ret = fnt5(dtc)

   if ( dtc%a /= 10 ) error stop 32
   if ( dtc%d1%a /= 10 ) error stop 34
   if ( dtc%d1%d0%a /= 10 ) error stop 36

!! Test 6

   dtc%a = 5
   dtc%d1%a = 5
   dtc%d1%d0%a = 5

   ret = fnt6(dtc)

   if ( dtc%a /= 5 ) error stop 38
   if ( dtc%d1%a /= 5 ) error stop 40
   if ( dtc%d1%d0%a /= 5 ) error stop 42

!! Test 7

   dta%a = 5

   ret = fnt7(dta)

   if ( dta%a /= 5 ) error stop 44

!! Test 8

   dta%a = 5

   ret = fnt8(dta)

   if ( dta%a /= 5 ) error stop 46

!! Test 9

   dtb%a = 5
   dtb%d0%a = 5

   ret = fnt9(dtb)

   if ( dtb%a /= 5 ) error stop 48
   if ( dtb%d0%a /= 5 ) error stop 50

!! Test 10

   dtb%a = 5
   dtb%d0%a = 5

   ret = fnt10(dtb)

   if ( dtb%a /= 5 ) error stop 52
   if ( dtb%d0%a /= 5 ) error stop 54

!! Test 11

   dtc%a = 5
   dtc%d1%a = 5
   dtc%d1%d0%a = 5

   ret = fnt11(dtc)

   if ( dtc%a /= 5 ) error stop 56
   if ( dtc%d1%a /= 5 ) error stop 58
   if ( dtc%d1%d0%a /= 5 ) error stop 60

!! Test 12

   dtc%a = 5
   dtc%d1%a = 5
   dtc%d1%d0%a = 5

   ret = fnt12(dtc)

   if ( dtc%a /= 5 ) error stop 62
   if ( dtc%d1%a /= 5 ) error stop 64
   if ( dtc%d1%d0%a /= 5 ) error stop 66

!! Test 13

   dta%a = 5

   ret = fnt13(dta)

   if ( dta%a /= 10 ) error stop 68

!! Test 14

   dtb%a = 5
   dtb%d0%a = 5

   ret = fnt14(dtb)

   if ( dtb%a /= 10 ) error stop 70
   if ( dtb%d0%a /= 10 ) error stop 72

!! Test 15

   dtc%a = 5
   dtc%d1%a = 5
   dtc%d1%d0%a = 5

   ret = fnt15(dtc)

   if ( dtc%a /= 10 ) error stop 74
   if ( dtc%d1%a /= 10 ) error stop 76
   if ( dtc%d1%d0%a /= 10 ) error stop 78

end

integer(C_INT_FAST16_T) function fnt1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt0), intent(inout) :: dt

   if ( dt%a /= 5 ) error stop 80

   dt%a = dt%a + 5

   fnt1 = 0
end function fnt1

integer(C_INT_FAST16_T) function fnt2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt0), value :: dt

   if ( dt%a /= 5 ) error stop 82

   dt%a = dt%a + 5

   fnt2 = 0
end function fnt2

integer(C_INT_FAST16_T) function fnt3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt1), intent(inout) :: dt

   if ( dt%a /= 5 ) error stop 84
   if ( dt%d0%a /= 5 ) error stop 86

   dt%a = dt%a + 5
   dt%d0%a = dt%d0%a + 5

   fnt3 = 0
end function fnt3

integer(C_INT_FAST16_T) function fnt4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt1), value :: dt

   if ( dt%a /= 5 ) error stop 88
   if ( dt%d0%a /= 5 ) error stop 90

   dt%a = dt%a + 5
   dt%d0%a = dt%d0%a + 5

   fnt4 = 0
end function fnt4

integer(C_INT_FAST16_T) function fnt5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt2), intent(inout) :: dt

   if ( dt%a /= 5 ) error stop 92
   if ( dt%d1%a /= 5 ) error stop 94
   if ( dt%d1%d0%a /= 5 ) error stop 96

   dt%a = dt%a + 5
   dt%d1%a = dt%d1%a + 5
   dt%d1%d0%a = dt%d1%d0%a + 5

   fnt5 = 0
end function fnt5

integer(C_INT_FAST16_T) function fnt6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt2), value :: dt

   if ( dt%a /= 5 ) error stop 98
   if ( dt%d1%a /= 5 ) error stop 100
   if ( dt%d1%d0%a /= 5 ) error stop 102

   dt%a = dt%a + 5
   dt%d1%a = dt%d1%a + 5
   dt%d1%d0%a = dt%d1%d0%a + 5

   fnt6 = 0
end function fnt6

integer(C_INT_FAST16_T) function fnt7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt0), intent(in) :: dt

   if ( dt%a /= 5 ) error stop 104

   fnt7 = 0
end function fnt7

integer(C_INT_FAST16_T) function fnt8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt0), intent(in), value :: dt

   if ( dt%a /= 5 ) error stop 106

   fnt8 = 0
end function fnt8

integer(C_INT_FAST16_T) function fnt9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt1), intent(in) :: dt

   if ( dt%a /= 5 ) error stop 108
   if ( dt%d0%a /= 5 ) error stop 110

   fnt9 = 0
end function fnt9

integer(C_INT_FAST16_T) function fnt10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt1), intent(in), value :: dt

   if ( dt%a /= 5 ) error stop 112
   if ( dt%d0%a /= 5 ) error stop 114

   fnt10 = 0
end function fnt10

integer(C_INT_FAST16_T) function fnt11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt2), intent(in) :: dt

   if ( dt%a /= 5 ) error stop 116
   if ( dt%d1%a /= 5 ) error stop 118
   if ( dt%d1%d0%a /= 5 ) error stop 120

   fnt11 = 0
end function fnt11

integer(C_INT_FAST16_T) function fnt12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt2), intent(in), value :: dt

   if ( dt%a /= 5 ) error stop 122
   if ( dt%d1%a /= 5 ) error stop 124
   if ( dt%d1%d0%a /= 5 ) error stop 126

   fnt12 = 0
end function fnt12

integer(C_INT_FAST16_T) function fnt13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt0), intent(out) :: dt

   dt%a = dt%a + 5

   fnt13 = 0
end function fnt13

integer(C_INT_FAST16_T) function fnt14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt1), intent(out) :: dt

   dt%a = dt%a + 5
   dt%d0%a = dt%d0%a + 5

   fnt14 = 0
end function fnt14

integer(C_INT_FAST16_T) function fnt15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt2), intent(out) :: dt

   dt%a = dt%a + 5
   dt%d1%a = dt%d1%a + 5
   dt%d1%d0%a = dt%d1%d0%a + 5

   fnt15 = 0
end function fnt15

