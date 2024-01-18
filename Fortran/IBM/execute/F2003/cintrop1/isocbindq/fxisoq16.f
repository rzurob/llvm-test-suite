!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoq00.presh fxisoq16 cxisoq16
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
!*  KEYWORD(S)                 : 16
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing 16
!*      - using C functions with interfaces to FORTRAN subroutines
!*      - passing derived types with scalar fields as arguments
!*      - testing INTENT and VALUE attributes
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob16
   use ISO_C_BINDING

   type, bind(c) :: dt0
      complex(16) :: a
   end type

   type, bind(c) :: dt1
      complex(16) :: a
      type(dt0) :: d0
   end type

   type, bind(c) :: dt2
      complex(16) :: a
      type(dt1) :: d1
   end type

end module mxisob16

program fxisoq16
   use ISO_C_BINDING
   use mxisob16

   interface
      subroutine sub1(dt) bind(c)
         use mxisob16
         type(dt0), intent(inout) :: dt
      end subroutine sub1
      subroutine sub2(dt) bind(c)
         use mxisob16
         type(dt0), value :: dt
      end subroutine sub2
      subroutine sub3(dt) bind(c)
         use mxisob16
         type(dt1), intent(inout) :: dt
      end subroutine sub3
      subroutine sub4(dt) bind(c)
         use mxisob16
         type(dt1), value :: dt
      end subroutine sub4
      subroutine sub5(dt) bind(c)
         use mxisob16
         type(dt2), intent(inout) :: dt
      end subroutine sub5
      subroutine sub6(dt) bind(c)
         use mxisob16
         type(dt2), value :: dt
      end subroutine sub6
      subroutine sub7(dt) bind(c)
         use mxisob16
         type(dt0), intent(in) :: dt
      end subroutine sub7
      subroutine sub7a(dt) bind(c)
         use mxisob16
         type(dt0), intent(in) :: dt
      end subroutine sub7a
      subroutine sub8(dt) bind(c)
         use mxisob16
         type(dt0), intent(in), value :: dt
      end subroutine sub8
      subroutine sub8a(dt) bind(c)
         use mxisob16
         type(dt0), intent(in), value :: dt
      end subroutine sub8a
      subroutine sub9(dt) bind(c)
         use mxisob16
         type(dt1), intent(in) :: dt
      end subroutine sub9
      subroutine sub9a(dt) bind(c)
         use mxisob16
         type(dt1), intent(in) :: dt
      end subroutine sub9a
      subroutine sub10(dt) bind(c)
         use mxisob16
         type(dt1), intent(in), value :: dt
      end subroutine sub10
      subroutine sub10a(dt) bind(c)
         use mxisob16
         type(dt1), intent(in), value :: dt
      end subroutine sub10a
      subroutine sub11(dt) bind(c)
         use mxisob16
         type(dt2), intent(in) :: dt
      end subroutine sub11
      subroutine sub11a(dt) bind(c)
         use mxisob16
         type(dt2), intent(in) :: dt
      end subroutine sub11a
      subroutine sub12(dt) bind(c)
         use mxisob16
         type(dt2), intent(in), value :: dt
      end subroutine sub12
      subroutine sub12a(dt) bind(c)
         use mxisob16
         type(dt2), intent(in), value :: dt
      end subroutine sub12a
      subroutine sub13(dt) bind(c)
         use mxisob16
         type(dt0), intent(out) :: dt
      end subroutine sub13
      subroutine sub14(dt) bind(c)
         use mxisob16
         type(dt1), intent(out) :: dt
      end subroutine sub14
      subroutine sub15(dt) bind(c)
         use mxisob16
         type(dt2), intent(out) :: dt
      end subroutine sub15
   end interface

   type(dt0) :: dta
   type(dt1) :: dtb
   type(dt2) :: dtc
   integer ret

!! Test 1

   dta%a = (5.0q0,5.0q0)

   call sub1(dta)

   if ( dta%a /= (10.0q0,10.0q0) ) error stop 20

!! Test 2

   dta%a = (5.0q0,5.0q0)

   call sub2(dta)

   if ( dta%a /= (5.0q0,5.0q0) ) error stop 22

!! Test 3

   dtb%a = (5.0q0,5.0q0)
   dtb%d0%a = (5.0q0,5.0q0)

   call sub3(dtb)

   if ( dtb%a /= (10.0q0,10.0q0) ) error stop 24
   if ( dtb%d0%a /= (10.0q0,10.0q0) ) error stop 26

!! Test 4

   dtb%a = (5.0q0,5.0q0)
   dtb%d0%a = (5.0q0,5.0q0)

   call sub4(dtb)

   if ( dtb%a /= (5.0q0,5.0q0) ) error stop 28
   if ( dtb%d0%a /= (5.0q0,5.0q0) ) error stop 30

!! Test 5

   dtc%a = (5.0q0,5.0q0)
   dtc%d1%a = (5.0q0,5.0q0)
   dtc%d1%d0%a = (5.0q0,5.0q0)

   call sub5(dtc)

   if ( dtc%a /= (10.0q0,10.0q0) ) error stop 32
   if ( dtc%d1%a /= (10.0q0,10.0q0) ) error stop 34
   if ( dtc%d1%d0%a /= (10.0q0,10.0q0) ) error stop 36

!! Test 6

   dtc%a = (5.0q0,5.0q0)
   dtc%d1%a = (5.0q0,5.0q0)
   dtc%d1%d0%a = (5.0q0,5.0q0)

   call sub6(dtc)

   if ( dtc%a /= (5.0q0,5.0q0) ) error stop 38
   if ( dtc%d1%a /= (5.0q0,5.0q0) ) error stop 40
   if ( dtc%d1%d0%a /= (5.0q0,5.0q0) ) error stop 42

!! Test 7

   dta%a = (5.0q0,5.0q0)

   call sub7(dta)

   if ( dta%a /= (5.0q0,5.0q0) ) error stop 44

!! Test 7a

   dta%a = (5.0q0,5.0q0)

   call sub7a(dta)

   if ( dta%a /= (5.0q0,5.0q0) ) error stop 46

!! Test 8

   dta%a = (5.0q0,5.0q0)

   call sub8(dta)

   if ( dta%a /= (5.0q0,5.0q0) ) error stop 48

!! Test 8a

   dta%a = (5.0q0,5.0q0)

   call sub8a(dta)

   if ( dta%a /= (5.0q0,5.0q0) ) error stop 50

!! Test 9

   dtb%a = (5.0q0,5.0q0)
   dtb%d0%a = (5.0q0,5.0q0)

   call sub9(dtb)

   if ( dtb%a /= (5.0q0,5.0q0) ) error stop 52
   if ( dtb%d0%a /= (5.0q0,5.0q0) ) error stop 54

!! Test 9a

   dtb%a = (5.0q0,5.0q0)
   dtb%d0%a = (5.0q0,5.0q0)

   call sub9a(dtb)

   if ( dtb%a /= (5.0q0,5.0q0) ) error stop 56
   if ( dtb%d0%a /= (5.0q0,5.0q0) ) error stop 58

!! Test 10

   dtb%a = (5.0q0,5.0q0)
   dtb%d0%a = (5.0q0,5.0q0)

   call sub10(dtb)

   if ( dtb%a /= (5.0q0,5.0q0) ) error stop 60
   if ( dtb%d0%a /= (5.0q0,5.0q0) ) error stop 62

!! Test 10a

   dtb%a = (5.0q0,5.0q0)
   dtb%d0%a = (5.0q0,5.0q0)

   call sub10a(dtb)

   if ( dtb%a /= (5.0q0,5.0q0) ) error stop 64
   if ( dtb%d0%a /= (5.0q0,5.0q0) ) error stop 66

!! Test 11

   dtc%a = (5.0q0,5.0q0)
   dtc%d1%a = (5.0q0,5.0q0)
   dtc%d1%d0%a = (5.0q0,5.0q0)

   call sub11(dtc)

   if ( dtc%a /= (5.0q0,5.0q0) ) error stop 68
   if ( dtc%d1%a /= (5.0q0,5.0q0) ) error stop 70
   if ( dtc%d1%d0%a /= (5.0q0,5.0q0) ) error stop 72

!! Test 11a

   dtc%a = (5.0q0,5.0q0)
   dtc%d1%a = (5.0q0,5.0q0)
   dtc%d1%d0%a = (5.0q0,5.0q0)

   call sub11a(dtc)

   if ( dtc%a /= (5.0q0,5.0q0) ) error stop 74
   if ( dtc%d1%a /= (5.0q0,5.0q0) ) error stop 76
   if ( dtc%d1%d0%a /= (5.0q0,5.0q0) ) error stop 78

!! Test 12

   dtc%a = (5.0q0,5.0q0)
   dtc%d1%a = (5.0q0,5.0q0)
   dtc%d1%d0%a = (5.0q0,5.0q0)

   call sub12(dtc)

   if ( dtc%a /= (5.0q0,5.0q0) ) error stop 80
   if ( dtc%d1%a /= (5.0q0,5.0q0) ) error stop 82
   if ( dtc%d1%d0%a /= (5.0q0,5.0q0) ) error stop 84

!! Test 12a

   dtc%a = (5.0q0,5.0q0)
   dtc%d1%a = (5.0q0,5.0q0)
   dtc%d1%d0%a = (5.0q0,5.0q0)

   call sub12a(dtc)

   if ( dtc%a /= (5.0q0,5.0q0) ) error stop 86
   if ( dtc%d1%a /= (5.0q0,5.0q0) ) error stop 88
   if ( dtc%d1%d0%a /= (5.0q0,5.0q0) ) error stop 90

!! Test 13

   dta%a = (5.0q0,5.0q0)

   call sub13(dta)

   if ( dta%a /= (10.0q0,10.0q0) ) error stop 92

!! Test 14

   dtb%a = (5.0q0,5.0q0)
   dtb%d0%a = (5.0q0,5.0q0)

   call sub14(dtb)

   if ( dtb%a /= (10.0q0,10.0q0) ) error stop 94
   if ( dtb%d0%a /= (10.0q0,10.0q0) ) error stop 96

!! Test 15

   dtc%a = (5.0q0,5.0q0)
   dtc%d1%a = (5.0q0,5.0q0)
   dtc%d1%d0%a = (5.0q0,5.0q0)

   call sub15(dtc)

   if ( dtc%a /= (10.0q0,10.0q0) ) error stop 98
   if ( dtc%d1%a /= (10.0q0,10.0q0) ) error stop 100
   if ( dtc%d1%d0%a /= (10.0q0,10.0q0) ) error stop 102

end
