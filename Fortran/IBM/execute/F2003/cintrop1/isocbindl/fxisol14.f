!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisol14 cxisol14
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
!*  KEYWORD(S)                 : C_FLOAT, C_DOUBLE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_FLOAT and C_DOUBLE
!*      - using C functions with interfaces to FORTRAN functions
!*      - passing derived types with scalar fields as arguments
!*      - testing INTENT and VALUE attributes
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob14
   use ISO_C_BINDING

   type, bind(c) :: dt0
      real(C_FLOAT) :: a
      real(C_DOUBLE) :: b
   end type

   type, bind(c) :: dt1
      real(C_FLOAT) :: a
      real(C_DOUBLE) :: b
      type(dt0) :: d0
   end type

   type, bind(c) :: dt2
      real(C_FLOAT) :: a
      real(C_DOUBLE) :: b
      type(dt1) :: d1
   end type

end module mxisob14

program fxisol14
   use ISO_C_BINDING
   use mxisob14

   interface
      real(C_FLOAT) function fnt1(dt) bind(c)
         use mxisob14
         type(dt0), intent(inout) :: dt
      end function fnt1
      real(C_FLOAT) function fnt2(dt) bind(c)
         use mxisob14
         type(dt0), value :: dt
      end function fnt2
      real(C_FLOAT) function fnt3(dt) bind(c)
         use mxisob14
         type(dt1), intent(inout) :: dt
      end function fnt3
      real(C_FLOAT) function fnt4(dt) bind(c)
         use mxisob14
         type(dt1), value :: dt
      end function fnt4
      real(C_FLOAT) function fnt5(dt) bind(c)
         use mxisob14
         type(dt2), intent(inout) :: dt
      end function fnt5
      real(C_FLOAT) function fnt6(dt) bind(c)
         use mxisob14
         type(dt2), value :: dt
      end function fnt6
      real(C_FLOAT) function fnt7(dt) bind(c)
         use mxisob14
         type(dt0), intent(in) :: dt
      end function fnt7
      real(C_FLOAT) function fnt7a(dt) bind(c)
         use mxisob14
         type(dt0), intent(in) :: dt
      end function fnt7a
      real(C_FLOAT) function fnt8(dt) bind(c)
         use mxisob14
         type(dt0), intent(in), value :: dt
      end function fnt8
      real(C_FLOAT) function fnt8a(dt) bind(c)
         use mxisob14
         type(dt0), intent(in), value :: dt
      end function fnt8a
      real(C_FLOAT) function fnt9(dt) bind(c)
         use mxisob14
         type(dt1), intent(in) :: dt
      end function fnt9
      real(C_FLOAT) function fnt9a(dt) bind(c)
         use mxisob14
         type(dt1), intent(in) :: dt
      end function fnt9a
      real(C_FLOAT) function fnt10(dt) bind(c)
         use mxisob14
         type(dt1), intent(in), value :: dt
      end function fnt10
      real(C_FLOAT) function fnt10a(dt) bind(c)
         use mxisob14
         type(dt1), intent(in), value :: dt
      end function fnt10a
      real(C_FLOAT) function fnt11(dt) bind(c)
         use mxisob14
         type(dt2), intent(in) :: dt
      end function fnt11
      real(C_FLOAT) function fnt11a(dt) bind(c)
         use mxisob14
         type(dt2), intent(in) :: dt
      end function fnt11a
      real(C_FLOAT) function fnt12(dt) bind(c)
         use mxisob14
         type(dt2), intent(in), value :: dt
      end function fnt12
      real(C_FLOAT) function fnt12a(dt) bind(c)
         use mxisob14
         type(dt2), intent(in), value :: dt
      end function fnt12a
      real(C_FLOAT) function fnt13(dt) bind(c)
         use mxisob14
         type(dt0), intent(out) :: dt
      end function fnt13
      real(C_FLOAT) function fnt14(dt) bind(c)
         use mxisob14
         type(dt1), intent(out) :: dt
      end function fnt14
      real(C_FLOAT) function fnt15(dt) bind(c)
         use mxisob14
         type(dt2), intent(out) :: dt
      end function fnt15
   end interface

   type(dt0) :: dta
   type(dt1) :: dtb
   type(dt2) :: dtc
   integer ret

!! Test 1

   dta%a = 5.0e0
   dta%b = 10.0d0

   ret = fnt1(dta)

   if ( dta%a /= 10.0e0 ) error stop 20
   if ( dta%b /= 20.0d0 ) error stop 22

!! Test 2

   dta%a = 5.0e0
   dta%b = 10.0d0

   ret = fnt2(dta)

   if ( dta%a /= 5.0e0 ) error stop 24
   if ( dta%b /= 10.0d0 ) error stop 26

!! Test 3

   dtb%a = 5.0e0
   dtb%b = 10.0d0
   dtb%d0%a = 5.0e0
   dtb%d0%b = 10.0d0

   ret = fnt3(dtb)

   if ( dtb%a /= 10.0e0 ) error stop 28
   if ( dtb%b /= 20.0d0 ) error stop 30
   if ( dtb%d0%a /= 10.0e0 ) error stop 32
   if ( dtb%d0%b /= 20.0d0 ) error stop 34

!! Test 4

   dtb%a = 5.0e0
   dtb%b = 10.0d0
   dtb%d0%a = 5.0e0
   dtb%d0%b = 10.0d0

   ret = fnt4(dtb)

   if ( dtb%a /= 5.0e0 ) error stop 36
   if ( dtb%b /= 10.0d0 ) error stop 38
   if ( dtb%d0%a /= 5.0e0 ) error stop 40
   if ( dtb%d0%b /= 10.0d0 ) error stop 42

!! Test 5

   dtc%a = 5.0e0
   dtc%b = 10.0d0
   dtc%d1%a = 5.0e0
   dtc%d1%b = 10.0d0
   dtc%d1%d0%a = 5.0e0
   dtc%d1%d0%b = 10.0d0

   ret = fnt5(dtc)

   if ( dtc%a /= 10.0e0 ) error stop 44
   if ( dtc%b /= 20.0d0 ) error stop 46
   if ( dtc%d1%a /= 10.0e0 ) error stop 48
   if ( dtc%d1%b /= 20.0d0 ) error stop 50
   if ( dtc%d1%d0%a /= 10.0e0 ) error stop 52
   if ( dtc%d1%d0%b /= 20.0d0 ) error stop 54

!! Test 6

   dtc%a = 5.0e0
   dtc%b = 10.0d0
   dtc%d1%a = 5.0e0
   dtc%d1%b = 10.0d0
   dtc%d1%d0%a = 5.0e0
   dtc%d1%d0%b = 10.0d0

   ret = fnt6(dtc)

   if ( dtc%a /= 5.0e0 ) error stop 56
   if ( dtc%b /= 10.0d0 ) error stop 58
   if ( dtc%d1%a /= 5.0e0 ) error stop 60
   if ( dtc%d1%b /= 10.0d0 ) error stop 62
   if ( dtc%d1%d0%a /= 5.0e0 ) error stop 64
   if ( dtc%d1%d0%b /= 10.0d0 ) error stop 66

!! Test 7

   dta%a = 5.0e0
   dta%b = 10.0d0

   ret = fnt7(dta)

   if ( dta%a /= 5.0e0 ) error stop 68
   if ( dta%b /= 10.0d0 ) error stop 70

!! Test 7a

   dta%a = 5.0e0
   dta%b = 10.0d0

   ret = fnt7a(dta)

   if ( dta%a /= 5.0e0 ) error stop 72
   if ( dta%b /= 10.0d0 ) error stop 74

!! Test 8

   dta%a = 5.0e0
   dta%b = 10.0d0

   ret = fnt8(dta)

   if ( dta%a /= 5.0e0 ) error stop 76
   if ( dta%b /= 10.0d0 ) error stop 78

!! Test 8a

   dta%a = 5.0e0
   dta%b = 10.0d0

   ret = fnt8a(dta)

   if ( dta%a /= 5.0e0 ) error stop 80
   if ( dta%b /= 10.0d0 ) error stop 82

!! Test 9

   dtb%a = 5.0e0
   dtb%b = 10.0d0
   dtb%d0%a = 5.0e0
   dtb%d0%b = 10.0d0

   ret = fnt9(dtb)

   if ( dtb%a /= 5.0e0 ) error stop 84
   if ( dtb%b /= 10.0d0 ) error stop 86
   if ( dtb%d0%a /= 5.0e0 ) error stop 88
   if ( dtb%d0%b /= 10.0d0 ) error stop 90

!! Test 9a

   dtb%a = 5.0e0
   dtb%b = 10.0d0
   dtb%d0%a = 5.0e0
   dtb%d0%b = 10.0d0

   ret = fnt9a(dtb)

   if ( dtb%a /= 5.0e0 ) error stop 92
   if ( dtb%b /= 10.0d0 ) error stop 94
   if ( dtb%d0%a /= 5.0e0 ) error stop 96
   if ( dtb%d0%b /= 10.0d0 ) error stop 98

!! Test 10

   dtb%a = 5.0e0
   dtb%b = 10.0d0
   dtb%d0%a = 5.0e0
   dtb%d0%b = 10.0d0

   ret = fnt10(dtb)

   if ( dtb%a /= 5.0e0 ) error stop 100
   if ( dtb%b /= 10.0d0 ) error stop 102
   if ( dtb%d0%a /= 5.0e0 ) error stop 104
   if ( dtb%d0%b /= 10.0d0 ) error stop 106

!! Test 10a

   dtb%a = 5.0e0
   dtb%b = 10.0d0
   dtb%d0%a = 5.0e0
   dtb%d0%b = 10.0d0

   ret = fnt10a(dtb)

   if ( dtb%a /= 5.0e0 ) error stop 108
   if ( dtb%b /= 10.0d0 ) error stop 110
   if ( dtb%d0%a /= 5.0e0 ) error stop 112
   if ( dtb%d0%b /= 10.0d0 ) error stop 114

!! Test 11

   dtc%a = 5.0e0
   dtc%b = 10.0d0
   dtc%d1%a = 5.0e0
   dtc%d1%b = 10.0d0
   dtc%d1%d0%a = 5.0e0
   dtc%d1%d0%b = 10.0d0

   ret = fnt11(dtc)

   if ( dtc%a /= 5.0e0 ) error stop 116
   if ( dtc%b /= 10.0d0 ) error stop 118
   if ( dtc%d1%a /= 5.0e0 ) error stop 120
   if ( dtc%d1%b /= 10.0d0 ) error stop 122
   if ( dtc%d1%d0%a /= 5.0e0 ) error stop 124
   if ( dtc%d1%d0%b /= 10.0d0 ) error stop 126

!! Test 11a

   dtc%a = 5.0e0
   dtc%b = 10.0d0
   dtc%d1%a = 5.0e0
   dtc%d1%b = 10.0d0
   dtc%d1%d0%a = 5.0e0
   dtc%d1%d0%b = 10.0d0

   ret = fnt11a(dtc)

   if ( dtc%a /= 5.0e0 ) error stop 128
   if ( dtc%b /= 10.0d0 ) error stop 130
   if ( dtc%d1%a /= 5.0e0 ) error stop 132
   if ( dtc%d1%b /= 10.0d0 ) error stop 134
   if ( dtc%d1%d0%a /= 5.0e0 ) error stop 136
   if ( dtc%d1%d0%b /= 10.0d0 ) error stop 138

!! Test 12

   dtc%a = 5.0e0
   dtc%b = 10.0d0
   dtc%d1%a = 5.0e0
   dtc%d1%b = 10.0d0
   dtc%d1%d0%a = 5.0e0
   dtc%d1%d0%b = 10.0d0

   ret = fnt12(dtc)

   if ( dtc%a /= 5.0e0 ) error stop 140
   if ( dtc%b /= 10.0d0 ) error stop 142
   if ( dtc%d1%a /= 5.0e0 ) error stop 144
   if ( dtc%d1%b /= 10.0d0 ) error stop 146
   if ( dtc%d1%d0%a /= 5.0e0 ) error stop 148
   if ( dtc%d1%d0%b /= 10.0d0 ) error stop 150

!! Test 12a

   dtc%a = 5.0e0
   dtc%b = 10.0d0
   dtc%d1%a = 5.0e0
   dtc%d1%b = 10.0d0
   dtc%d1%d0%a = 5.0e0
   dtc%d1%d0%b = 10.0d0

   ret = fnt12a(dtc)

   if ( dtc%a /= 5.0e0 ) error stop 152
   if ( dtc%b /= 10.0d0 ) error stop 154
   if ( dtc%d1%a /= 5.0e0 ) error stop 156
   if ( dtc%d1%b /= 10.0d0 ) error stop 158
   if ( dtc%d1%d0%a /= 5.0e0 ) error stop 160
   if ( dtc%d1%d0%b /= 10.0d0 ) error stop 162

!! Test 13

   dta%a = 5.0e0
   dta%b = 10.0d0

   ret = fnt13(dta)

   if ( dta%a /= 10.0e0 ) error stop 164
   if ( dta%b /= 20.0d0 ) error stop 166

!! Test 14

   dtb%a = 5.0e0
   dtb%b = 10.0d0
   dtb%d0%a = 5.0e0
   dtb%d0%b = 10.0d0

   ret = fnt14(dtb)

   if ( dtb%a /= 10.0e0 ) error stop 168
   if ( dtb%b /= 20.0d0 ) error stop 170
   if ( dtb%d0%a /= 10.0e0 ) error stop 172
   if ( dtb%d0%b /= 20.0d0 ) error stop 174

!! Test 15

   dtc%a = 5.0e0
   dtc%b = 10.0d0
   dtc%d1%a = 5.0e0
   dtc%d1%b = 10.0d0
   dtc%d1%d0%a = 5.0e0
   dtc%d1%d0%b = 10.0d0

   ret = fnt15(dtc)

   if ( dtc%a /= 10.0e0 ) error stop 176
   if ( dtc%b /= 20.0d0 ) error stop 178
   if ( dtc%d1%a /= 10.0e0 ) error stop 180
   if ( dtc%d1%b /= 20.0d0 ) error stop 182
   if ( dtc%d1%d0%a /= 10.0e0 ) error stop 184
   if ( dtc%d1%d0%b /= 20.0d0 ) error stop 186

end
