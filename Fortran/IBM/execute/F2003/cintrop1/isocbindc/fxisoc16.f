!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisoc16 cxisoc16
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
!*  KEYWORD(S)                 : C_INT8_T, C_INT16_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_INT8_T and C_INT16_T
!*      - using C functions with interfaces to FORTRAN subroutines
!*      - passing derived types with scalar fields as arguments
!*      - testing INTENT and VALUE attributes
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob16
   use ISO_C_BINDING

   type, bind(c) :: dt0
      integer(C_INT8_T) :: a
      integer(C_INT16_T) :: b
   end type

   type, bind(c) :: dt1
      integer(C_INT8_T) :: a
      integer(C_INT16_T) :: b
      type(dt0) :: d0
   end type

   type, bind(c) :: dt2
      integer(C_INT8_T) :: a
      integer(C_INT16_T) :: b
      type(dt1) :: d1
   end type

end module mxisob16

program fxisoc16
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

   dta%a = 5
   dta%b = 10

   call sub1(dta)

   if ( dta%a /= 10 ) error stop 20
   if ( dta%b /= 20 ) error stop 22

!! Test 2

   dta%a = 5
   dta%b = 10

   call sub2(dta)

   if ( dta%a /= 5 ) error stop 24
   if ( dta%b /= 10 ) error stop 26

!! Test 3

   dtb%a = 5
   dtb%b = 10
   dtb%d0%a = 5
   dtb%d0%b = 10

   call sub3(dtb)

   if ( dtb%a /= 10 ) error stop 28
   if ( dtb%b /= 20 ) error stop 30
   if ( dtb%d0%a /= 10 ) error stop 32
   if ( dtb%d0%b /= 20 ) error stop 34

!! Test 4

   dtb%a = 5
   dtb%b = 10
   dtb%d0%a = 5
   dtb%d0%b = 10

   call sub4(dtb)

   if ( dtb%a /= 5 ) error stop 36
   if ( dtb%b /= 10 ) error stop 38
   if ( dtb%d0%a /= 5 ) error stop 40
   if ( dtb%d0%b /= 10 ) error stop 42

!! Test 5

   dtc%a = 5
   dtc%b = 10
   dtc%d1%a = 5
   dtc%d1%b = 10
   dtc%d1%d0%a = 5
   dtc%d1%d0%b = 10

   call sub5(dtc)

   if ( dtc%a /= 10 ) error stop 44
   if ( dtc%b /= 20 ) error stop 46
   if ( dtc%d1%a /= 10 ) error stop 48
   if ( dtc%d1%b /= 20 ) error stop 50
   if ( dtc%d1%d0%a /= 10 ) error stop 52
   if ( dtc%d1%d0%b /= 20 ) error stop 54

!! Test 6

   dtc%a = 5
   dtc%b = 10
   dtc%d1%a = 5
   dtc%d1%b = 10
   dtc%d1%d0%a = 5
   dtc%d1%d0%b = 10

   call sub6(dtc)

   if ( dtc%a /= 5 ) error stop 56
   if ( dtc%b /= 10 ) error stop 58
   if ( dtc%d1%a /= 5 ) error stop 60
   if ( dtc%d1%b /= 10 ) error stop 62
   if ( dtc%d1%d0%a /= 5 ) error stop 64
   if ( dtc%d1%d0%b /= 10 ) error stop 66

!! Test 7

   dta%a = 5
   dta%b = 10

   call sub7(dta)

   if ( dta%a /= 5 ) error stop 68
   if ( dta%b /= 10 ) error stop 70

!! Test 7a

   dta%a = 5
   dta%b = 10

   call sub7a(dta)

   if ( dta%a /= 5 ) error stop 72
   if ( dta%b /= 10 ) error stop 74

!! Test 8

   dta%a = 5
   dta%b = 10

   call sub8(dta)

   if ( dta%a /= 5 ) error stop 76
   if ( dta%b /= 10 ) error stop 78

!! Test 8a

   dta%a = 5
   dta%b = 10

   call sub8a(dta)

   if ( dta%a /= 5 ) error stop 80
   if ( dta%b /= 10 ) error stop 82

!! Test 9

   dtb%a = 5
   dtb%b = 10
   dtb%d0%a = 5
   dtb%d0%b = 10

   call sub9(dtb)

   if ( dtb%a /= 5 ) error stop 84
   if ( dtb%b /= 10 ) error stop 86
   if ( dtb%d0%a /= 5 ) error stop 88
   if ( dtb%d0%b /= 10 ) error stop 90

!! Test 9a

   dtb%a = 5
   dtb%b = 10
   dtb%d0%a = 5
   dtb%d0%b = 10

   call sub9a(dtb)

   if ( dtb%a /= 5 ) error stop 92
   if ( dtb%b /= 10 ) error stop 94
   if ( dtb%d0%a /= 5 ) error stop 96
   if ( dtb%d0%b /= 10 ) error stop 98

!! Test 10

   dtb%a = 5
   dtb%b = 10
   dtb%d0%a = 5
   dtb%d0%b = 10

   call sub10(dtb)

   if ( dtb%a /= 5 ) error stop 100
   if ( dtb%b /= 10 ) error stop 102
   if ( dtb%d0%a /= 5 ) error stop 104
   if ( dtb%d0%b /= 10 ) error stop 106

!! Test 10a

   dtb%a = 5
   dtb%b = 10
   dtb%d0%a = 5
   dtb%d0%b = 10

   call sub10a(dtb)

   if ( dtb%a /= 5 ) error stop 108
   if ( dtb%b /= 10 ) error stop 110
   if ( dtb%d0%a /= 5 ) error stop 112
   if ( dtb%d0%b /= 10 ) error stop 114

!! Test 11

   dtc%a = 5
   dtc%b = 10
   dtc%d1%a = 5
   dtc%d1%b = 10
   dtc%d1%d0%a = 5
   dtc%d1%d0%b = 10

   call sub11(dtc)

   if ( dtc%a /= 5 ) error stop 116
   if ( dtc%b /= 10 ) error stop 118
   if ( dtc%d1%a /= 5 ) error stop 120
   if ( dtc%d1%b /= 10 ) error stop 122
   if ( dtc%d1%d0%a /= 5 ) error stop 124
   if ( dtc%d1%d0%b /= 10 ) error stop 126

!! Test 11a

   dtc%a = 5
   dtc%b = 10
   dtc%d1%a = 5
   dtc%d1%b = 10
   dtc%d1%d0%a = 5
   dtc%d1%d0%b = 10

   call sub11a(dtc)

   if ( dtc%a /= 5 ) error stop 128
   if ( dtc%b /= 10 ) error stop 130
   if ( dtc%d1%a /= 5 ) error stop 132
   if ( dtc%d1%b /= 10 ) error stop 134
   if ( dtc%d1%d0%a /= 5 ) error stop 136
   if ( dtc%d1%d0%b /= 10 ) error stop 138

!! Test 12

   dtc%a = 5
   dtc%b = 10
   dtc%d1%a = 5
   dtc%d1%b = 10
   dtc%d1%d0%a = 5
   dtc%d1%d0%b = 10

   call sub12(dtc)

   if ( dtc%a /= 5 ) error stop 140
   if ( dtc%b /= 10 ) error stop 142
   if ( dtc%d1%a /= 5 ) error stop 144
   if ( dtc%d1%b /= 10 ) error stop 146
   if ( dtc%d1%d0%a /= 5 ) error stop 148
   if ( dtc%d1%d0%b /= 10 ) error stop 150

!! Test 12a

   dtc%a = 5
   dtc%b = 10
   dtc%d1%a = 5
   dtc%d1%b = 10
   dtc%d1%d0%a = 5
   dtc%d1%d0%b = 10

   call sub12a(dtc)

   if ( dtc%a /= 5 ) error stop 152
   if ( dtc%b /= 10 ) error stop 154
   if ( dtc%d1%a /= 5 ) error stop 156
   if ( dtc%d1%b /= 10 ) error stop 158
   if ( dtc%d1%d0%a /= 5 ) error stop 160
   if ( dtc%d1%d0%b /= 10 ) error stop 162

!! Test 13

   dta%a = 5
   dta%b = 10

   call sub13(dta)

   if ( dta%a /= 10 ) error stop 164
   if ( dta%b /= 20 ) error stop 166

!! Test 14

   dtb%a = 5
   dtb%b = 10
   dtb%d0%a = 5
   dtb%d0%b = 10

   call sub14(dtb)

   if ( dtb%a /= 10 ) error stop 168
   if ( dtb%b /= 20 ) error stop 170
   if ( dtb%d0%a /= 10 ) error stop 172
   if ( dtb%d0%b /= 20 ) error stop 174

!! Test 15

   dtc%a = 5
   dtc%b = 10
   dtc%d1%a = 5
   dtc%d1%b = 10
   dtc%d1%d0%a = 5
   dtc%d1%d0%b = 10

   call sub15(dtc)

   if ( dtc%a /= 10 ) error stop 176
   if ( dtc%b /= 20 ) error stop 178
   if ( dtc%d1%a /= 10 ) error stop 180
   if ( dtc%d1%b /= 20 ) error stop 182
   if ( dtc%d1%d0%a /= 10 ) error stop 184
   if ( dtc%d1%d0%b /= 20 ) error stop 186

end
