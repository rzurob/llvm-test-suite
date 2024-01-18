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
!*  KEYWORD(S)                 : C_INT_LEAST8_T, C_INT_LEAST16_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_INT_LEAST8_T and C_INT_LEAST16_T
!*      - using C functions with interfaces to FORTRAN functions
!*      - function interfaces defined in module
!*      - passing derived types with scalar fields as arguments
!*      - testing INTENT and VALUE attributes
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob15
   use ISO_C_BINDING

   type, bind(c) :: dt0
      integer(C_INT_LEAST8_T) :: a
      integer(C_INT_LEAST16_T) :: b
   end type

   type, bind(c) :: dt1
      integer(C_INT_LEAST8_T) :: a
      integer(C_INT_LEAST16_T) :: b
      type(dt0) :: d0
   end type

   type, bind(c) :: dt2
      integer(C_INT_LEAST8_T) :: a
      integer(C_INT_LEAST16_T) :: b
      type(dt1) :: d1
   end type

end module mxisob15

module mxisob15a
   interface
      integer(C_INT_LEAST8_T) function fnt1(dt) bind(c)
         use mxisob15
         type(dt0), intent(inout) :: dt
      end function fnt1
      integer(C_INT_LEAST8_T) function fnt2(dt) bind(c)
         use mxisob15
         type(dt0), value :: dt
      end function fnt2
      integer(C_INT_LEAST8_T) function fnt3(dt) bind(c)
         use mxisob15
         type(dt1), intent(inout) :: dt
      end function fnt3
      integer(C_INT_LEAST8_T) function fnt4(dt) bind(c)
         use mxisob15
         type(dt1), value :: dt
      end function fnt4
      integer(C_INT_LEAST8_T) function fnt5(dt) bind(c)
         use mxisob15
         type(dt2), intent(inout) :: dt
      end function fnt5
      integer(C_INT_LEAST8_T) function fnt6(dt) bind(c)
         use mxisob15
         type(dt2), value :: dt
      end function fnt6
      integer(C_INT_LEAST8_T) function fnt7(dt) bind(c)
         use mxisob15
         type(dt0), intent(in) :: dt
      end function fnt7
      integer(C_INT_LEAST8_T) function fnt7a(dt) bind(c)
         use mxisob15
         type(dt0), intent(in) :: dt
      end function fnt7a
      integer(C_INT_LEAST8_T) function fnt8(dt) bind(c)
         use mxisob15
         type(dt0), intent(in), value :: dt
      end function fnt8
      integer(C_INT_LEAST8_T) function fnt8a(dt) bind(c)
         use mxisob15
         type(dt0), intent(in), value :: dt
      end function fnt8a
      integer(C_INT_LEAST8_T) function fnt9(dt) bind(c)
         use mxisob15
         type(dt1), intent(in) :: dt
      end function fnt9
      integer(C_INT_LEAST8_T) function fnt9a(dt) bind(c)
         use mxisob15
         type(dt1), intent(in) :: dt
      end function fnt9a
      integer(C_INT_LEAST8_T) function fnt10(dt) bind(c)
         use mxisob15
         type(dt1), intent(in), value :: dt
      end function fnt10
      integer(C_INT_LEAST8_T) function fnt10a(dt) bind(c)
         use mxisob15
         type(dt1), intent(in), value :: dt
      end function fnt10a
      integer(C_INT_LEAST8_T) function fnt11(dt) bind(c)
         use mxisob15
         type(dt2), intent(in) :: dt
      end function fnt11
      integer(C_INT_LEAST8_T) function fnt11a(dt) bind(c)
         use mxisob15
         type(dt2), intent(in) :: dt
      end function fnt11a
      integer(C_INT_LEAST8_T) function fnt12(dt) bind(c)
         use mxisob15
         type(dt2), intent(in), value :: dt
      end function fnt12
      integer(C_INT_LEAST8_T) function fnt12a(dt) bind(c)
         use mxisob15
         type(dt2), intent(in), value :: dt
      end function fnt12a
      integer(C_INT_LEAST8_T) function fnt13(dt) bind(c)
         use mxisob15
         type(dt0), intent(out) :: dt
      end function fnt13
      integer(C_INT_LEAST8_T) function fnt14(dt) bind(c)
         use mxisob15
         type(dt1), intent(out) :: dt
      end function fnt14
      integer(C_INT_LEAST8_T) function fnt15(dt) bind(c)
         use mxisob15
         type(dt2), intent(out) :: dt
      end function fnt15
   end interface
end module mxisob15a

program fxisoe15
   use ISO_C_BINDING
   use mxisob15
   use mxisob15a

   type(dt0) :: dta
   type(dt1) :: dtb
   type(dt2) :: dtc
   integer ret

!! Test 1

   dta%a = 5
   dta%b = 10

   ret = fnt1(dta)

   if ( dta%a /= 10 ) error stop 20
   if ( dta%b /= 20 ) error stop 22

!! Test 2

   dta%a = 5
   dta%b = 10

   ret = fnt2(dta)

   if ( dta%a /= 5 ) error stop 24
   if ( dta%b /= 10 ) error stop 26

!! Test 3

   dtb%a = 5
   dtb%b = 10
   dtb%d0%a = 5
   dtb%d0%b = 10

   ret = fnt3(dtb)

   if ( dtb%a /= 10 ) error stop 28
   if ( dtb%b /= 20 ) error stop 30
   if ( dtb%d0%a /= 10 ) error stop 32
   if ( dtb%d0%b /= 20 ) error stop 34

!! Test 4

   dtb%a = 5
   dtb%b = 10
   dtb%d0%a = 5
   dtb%d0%b = 10

   ret = fnt4(dtb)

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

   ret = fnt5(dtc)

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

   ret = fnt6(dtc)

   if ( dtc%a /= 5 ) error stop 56
   if ( dtc%b /= 10 ) error stop 58
   if ( dtc%d1%a /= 5 ) error stop 60
   if ( dtc%d1%b /= 10 ) error stop 62
   if ( dtc%d1%d0%a /= 5 ) error stop 64
   if ( dtc%d1%d0%b /= 10 ) error stop 66

!! Test 7

   dta%a = 5
   dta%b = 10

   ret = fnt7(dta)

   if ( dta%a /= 5 ) error stop 68
   if ( dta%b /= 10 ) error stop 70

!! Test 7a

   dta%a = 5
   dta%b = 10

   ret = fnt7a(dta)

   if ( dta%a /= 5 ) error stop 72
   if ( dta%b /= 10 ) error stop 74

!! Test 8

   dta%a = 5
   dta%b = 10

   ret = fnt8(dta)

   if ( dta%a /= 5 ) error stop 76
   if ( dta%b /= 10 ) error stop 78

!! Test 8a

   dta%a = 5
   dta%b = 10

   ret = fnt8a(dta)

   if ( dta%a /= 5 ) error stop 80
   if ( dta%b /= 10 ) error stop 82

!! Test 9

   dtb%a = 5
   dtb%b = 10
   dtb%d0%a = 5
   dtb%d0%b = 10

   ret = fnt9(dtb)

   if ( dtb%a /= 5 ) error stop 84
   if ( dtb%b /= 10 ) error stop 86
   if ( dtb%d0%a /= 5 ) error stop 88
   if ( dtb%d0%b /= 10 ) error stop 90

!! Test 9a

   dtb%a = 5
   dtb%b = 10
   dtb%d0%a = 5
   dtb%d0%b = 10

   ret = fnt9a(dtb)

   if ( dtb%a /= 5 ) error stop 92
   if ( dtb%b /= 10 ) error stop 94
   if ( dtb%d0%a /= 5 ) error stop 96
   if ( dtb%d0%b /= 10 ) error stop 98

!! Test 10

   dtb%a = 5
   dtb%b = 10
   dtb%d0%a = 5
   dtb%d0%b = 10

   ret = fnt10(dtb)

   if ( dtb%a /= 5 ) error stop 100
   if ( dtb%b /= 10 ) error stop 102
   if ( dtb%d0%a /= 5 ) error stop 104
   if ( dtb%d0%b /= 10 ) error stop 106

!! Test 10a

   dtb%a = 5
   dtb%b = 10
   dtb%d0%a = 5
   dtb%d0%b = 10

   ret = fnt10a(dtb)

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

   ret = fnt11(dtc)

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

   ret = fnt11a(dtc)

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

   ret = fnt12(dtc)

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

   ret = fnt12a(dtc)

   if ( dtc%a /= 5 ) error stop 152
   if ( dtc%b /= 10 ) error stop 154
   if ( dtc%d1%a /= 5 ) error stop 156
   if ( dtc%d1%b /= 10 ) error stop 158
   if ( dtc%d1%d0%a /= 5 ) error stop 160
   if ( dtc%d1%d0%b /= 10 ) error stop 162

!! Test 13

   dta%a = 5
   dta%b = 10

   ret = fnt13(dta)

   if ( dta%a /= 10 ) error stop 164
   if ( dta%b /= 20 ) error stop 166

!! Test 14

   dtb%a = 5
   dtb%b = 10
   dtb%d0%a = 5
   dtb%d0%b = 10

   ret = fnt14(dtb)

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

   ret = fnt15(dtc)

   if ( dtc%a /= 10 ) error stop 176
   if ( dtc%b /= 20 ) error stop 178
   if ( dtc%d1%a /= 10 ) error stop 180
   if ( dtc%d1%b /= 20 ) error stop 182
   if ( dtc%d1%d0%a /= 10 ) error stop 184
   if ( dtc%d1%d0%b /= 20 ) error stop 186

end
