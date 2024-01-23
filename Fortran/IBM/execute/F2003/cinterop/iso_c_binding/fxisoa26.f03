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
!*  KEYWORD(S)                 : C_INT, C_SHORT
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_INT and C_SHORT
!*      - FORTRAN code only
!*      - passing derived types with scalar fields as arguments
!*      - testing INTENT and VALUE attributes
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob26
   use ISO_C_BINDING

   type, bind(c) :: dt0
      integer(C_INT) :: a
      integer(C_SHORT) :: b
   end type

   type, bind(c) :: dt1
      integer(C_INT) :: a
      integer(C_SHORT) :: b
      type(dt0) :: d0
   end type

   type, bind(c) :: dt2
      integer(C_INT) :: a
      integer(C_SHORT) :: b
      type(dt1) :: d1
   end type

end module mxisob26

program fxisoa26
   use ISO_C_BINDING
   use mxisob26

   interface
      integer(C_INT) function fnt1(dt) bind(c)
         use mxisob26
         type(dt0), intent(inout) :: dt
      end function fnt1
      integer(C_INT) function fnt2(dt) bind(c)
         use mxisob26
         type(dt0), value :: dt
      end function fnt2
      integer(C_INT) function fnt3(dt) bind(c)
         use mxisob26
         type(dt1), intent(inout) :: dt
      end function fnt3
      integer(C_INT) function fnt4(dt) bind(c)
         use mxisob26
         type(dt1), value :: dt
      end function fnt4
      integer(C_INT) function fnt5(dt) bind(c)
         use mxisob26
         type(dt2), intent(inout) :: dt
      end function fnt5
      integer(C_INT) function fnt6(dt) bind(c)
         use mxisob26
         type(dt2), value :: dt
      end function fnt6
      integer(C_INT) function fnt7(dt) bind(c)
         use mxisob26
         type(dt0), intent(in) :: dt
      end function fnt7
      integer(C_INT) function fnt8(dt) bind(c)
         use mxisob26
         type(dt0), intent(in), value :: dt
      end function fnt8
      integer(C_INT) function fnt9(dt) bind(c)
         use mxisob26
         type(dt1), intent(in) :: dt
      end function fnt9
      integer(C_INT) function fnt10(dt) bind(c)
         use mxisob26
         type(dt1), intent(in), value :: dt
      end function fnt10
      integer(C_INT) function fnt11(dt) bind(c)
         use mxisob26
         type(dt2), intent(in) :: dt
      end function fnt11
      integer(C_INT) function fnt12(dt) bind(c)
         use mxisob26
         type(dt2), intent(in), value :: dt
      end function fnt12
      integer(C_INT) function fnt13(dt) bind(c)
         use mxisob26
         type(dt0), intent(out) :: dt
      end function fnt13
      integer(C_INT) function fnt14(dt) bind(c)
         use mxisob26
         type(dt1), intent(out) :: dt
      end function fnt14
      integer(C_INT) function fnt15(dt) bind(c)
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

!! Test 8

   dta%a = 5
   dta%b = 10

   ret = fnt8(dta)

   if ( dta%a /= 5 ) error stop 72
   if ( dta%b /= 10 ) error stop 74

!! Test 9

   dtb%a = 5
   dtb%b = 10
   dtb%d0%a = 5
   dtb%d0%b = 10

   ret = fnt9(dtb)

   if ( dtb%a /= 5 ) error stop 76
   if ( dtb%b /= 10 ) error stop 78
   if ( dtb%d0%a /= 5 ) error stop 80
   if ( dtb%d0%b /= 10 ) error stop 82

!! Test 10

   dtb%a = 5
   dtb%b = 10
   dtb%d0%a = 5
   dtb%d0%b = 10

   ret = fnt10(dtb)

   if ( dtb%a /= 5 ) error stop 84
   if ( dtb%b /= 10 ) error stop 86
   if ( dtb%d0%a /= 5 ) error stop 88
   if ( dtb%d0%b /= 10 ) error stop 90

!! Test 11

   dtc%a = 5
   dtc%b = 10
   dtc%d1%a = 5
   dtc%d1%b = 10
   dtc%d1%d0%a = 5
   dtc%d1%d0%b = 10

   ret = fnt11(dtc)

   if ( dtc%a /= 5 ) error stop 92
   if ( dtc%b /= 10 ) error stop 94
   if ( dtc%d1%a /= 5 ) error stop 96
   if ( dtc%d1%b /= 10 ) error stop 98
   if ( dtc%d1%d0%a /= 5 ) error stop 100
   if ( dtc%d1%d0%b /= 10 ) error stop 102

!! Test 12

   dtc%a = 5
   dtc%b = 10
   dtc%d1%a = 5
   dtc%d1%b = 10
   dtc%d1%d0%a = 5
   dtc%d1%d0%b = 10

   ret = fnt12(dtc)

   if ( dtc%a /= 5 ) error stop 104
   if ( dtc%b /= 10 ) error stop 106
   if ( dtc%d1%a /= 5 ) error stop 108
   if ( dtc%d1%b /= 10 ) error stop 110
   if ( dtc%d1%d0%a /= 5 ) error stop 112
   if ( dtc%d1%d0%b /= 10 ) error stop 114

!! Test 13

   dta%a = 5
   dta%b = 10

   ret = fnt13(dta)

   if ( dta%a /= 10 ) error stop 116
   if ( dta%b /= 20 ) error stop 118

!! Test 14

   dtb%a = 5
   dtb%b = 10
   dtb%d0%a = 5
   dtb%d0%b = 10

   ret = fnt14(dtb)

   if ( dtb%a /= 10 ) error stop 120
   if ( dtb%b /= 20 ) error stop 122
   if ( dtb%d0%a /= 10 ) error stop 124
   if ( dtb%d0%b /= 20 ) error stop 126

!! Test 15

   dtc%a = 5
   dtc%b = 10
   dtc%d1%a = 5
   dtc%d1%b = 10
   dtc%d1%d0%a = 5
   dtc%d1%d0%b = 10

   ret = fnt15(dtc)

   if ( dtc%a /= 10 ) error stop 128
   if ( dtc%b /= 20 ) error stop 130
   if ( dtc%d1%a /= 10 ) error stop 132
   if ( dtc%d1%b /= 20 ) error stop 134
   if ( dtc%d1%d0%a /= 10 ) error stop 136
   if ( dtc%d1%d0%b /= 20 ) error stop 138

end

integer(C_INT) function fnt1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt0), intent(inout) :: dt

   if ( dt%a /= 5 ) error stop 140
   if ( dt%b /= 10 ) error stop 142

   dt%a = dt%a + 5
   dt%b = dt%b + 10

   fnt1 = 0
end function fnt1

integer(C_INT) function fnt2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt0), value :: dt

   if ( dt%a /= 5 ) error stop 144
   if ( dt%b /= 10 ) error stop 146

   dt%a = dt%a + 5
   dt%b = dt%b + 10

   fnt2 = 0
end function fnt2

integer(C_INT) function fnt3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt1), intent(inout) :: dt

   if ( dt%a /= 5 ) error stop 148
   if ( dt%b /= 10 ) error stop 150
   if ( dt%d0%a /= 5 ) error stop 152
   if ( dt%d0%b /= 10 ) error stop 154

   dt%a = dt%a + 5
   dt%b = dt%b + 10
   dt%d0%a = dt%d0%a + 5
   dt%d0%b = dt%d0%b + 10

   fnt3 = 0
end function fnt3

integer(C_INT) function fnt4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt1), value :: dt

   if ( dt%a /= 5 ) error stop 156
   if ( dt%b /= 10 ) error stop 158
   if ( dt%d0%a /= 5 ) error stop 160
   if ( dt%d0%b /= 10 ) error stop 162

   dt%a = dt%a + 5
   dt%b = dt%b + 10
   dt%d0%a = dt%d0%a + 5
   dt%d0%b = dt%d0%b + 10

   fnt4 = 0
end function fnt4

integer(C_INT) function fnt5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt2), intent(inout) :: dt

   if ( dt%a /= 5 ) error stop 164
   if ( dt%b /= 10 ) error stop 166
   if ( dt%d1%a /= 5 ) error stop 168
   if ( dt%d1%b /= 10 ) error stop 170
   if ( dt%d1%d0%a /= 5 ) error stop 172
   if ( dt%d1%d0%b /= 10 ) error stop 174

   dt%a = dt%a + 5
   dt%b = dt%b + 10
   dt%d1%a = dt%d1%a + 5
   dt%d1%b = dt%d1%b + 10
   dt%d1%d0%a = dt%d1%d0%a + 5
   dt%d1%d0%b = dt%d1%d0%b + 10

   fnt5 = 0
end function fnt5

integer(C_INT) function fnt6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt2), value :: dt

   if ( dt%a /= 5 ) error stop 176
   if ( dt%b /= 10 ) error stop 178
   if ( dt%d1%a /= 5 ) error stop 180
   if ( dt%d1%b /= 10 ) error stop 182
   if ( dt%d1%d0%a /= 5 ) error stop 184
   if ( dt%d1%d0%b /= 10 ) error stop 186

   dt%a = dt%a + 5
   dt%b = dt%b + 10
   dt%d1%a = dt%d1%a + 5
   dt%d1%b = dt%d1%b + 10
   dt%d1%d0%a = dt%d1%d0%a + 5
   dt%d1%d0%b = dt%d1%d0%b + 10

   fnt6 = 0
end function fnt6

integer(C_INT) function fnt7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt0), intent(in) :: dt

   if ( dt%a /= 5 ) error stop 188
   if ( dt%b /= 10 ) error stop 190

   fnt7 = 0
end function fnt7

integer(C_INT) function fnt8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt0), intent(in), value :: dt

   if ( dt%a /= 5 ) error stop 192
   if ( dt%b /= 10 ) error stop 194

   fnt8 = 0
end function fnt8

integer(C_INT) function fnt9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt1), intent(in) :: dt

   if ( dt%a /= 5 ) error stop 196
   if ( dt%b /= 10 ) error stop 198
   if ( dt%d0%a /= 5 ) error stop 200
   if ( dt%d0%b /= 10 ) error stop 202

   fnt9 = 0
end function fnt9

integer(C_INT) function fnt10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt1), intent(in), value :: dt

   if ( dt%a /= 5 ) error stop 204
   if ( dt%b /= 10 ) error stop 206
   if ( dt%d0%a /= 5 ) error stop 208
   if ( dt%d0%b /= 10 ) error stop 210

   fnt10 = 0
end function fnt10

integer(C_INT) function fnt11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt2), intent(in) :: dt

   if ( dt%a /= 5 ) error stop 212
   if ( dt%b /= 10 ) error stop 214
   if ( dt%d1%a /= 5 ) error stop 216
   if ( dt%d1%b /= 10 ) error stop 218
   if ( dt%d1%d0%a /= 5 ) error stop 220
   if ( dt%d1%d0%b /= 10 ) error stop 222

   fnt11 = 0
end function fnt11

integer(C_INT) function fnt12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt2), intent(in), value :: dt

   if ( dt%a /= 5 ) error stop 224
   if ( dt%b /= 10 ) error stop 226
   if ( dt%d1%a /= 5 ) error stop 228
   if ( dt%d1%b /= 10 ) error stop 230
   if ( dt%d1%d0%a /= 5 ) error stop 232
   if ( dt%d1%d0%b /= 10 ) error stop 234

   fnt12 = 0
end function fnt12

integer(C_INT) function fnt13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt0), intent(out) :: dt

   dt%a = dt%a + 5
   dt%b = dt%b + 10

   fnt13 = 0
end function fnt13

integer(C_INT) function fnt14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt1), intent(out) :: dt

   dt%a = dt%a + 5
   dt%b = dt%b + 10
   dt%d0%a = dt%d0%a + 5
   dt%d0%b = dt%d0%b + 10

   fnt14 = 0
end function fnt14

integer(C_INT) function fnt15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob26

   type(dt2), intent(out) :: dt

   dt%a = dt%a + 5
   dt%b = dt%b + 10
   dt%d1%a = dt%d1%a + 5
   dt%d1%b = dt%d1%b + 10
   dt%d1%d0%a = dt%d1%d0%a + 5
   dt%d1%d0%b = dt%d1%d0%b + 10

   fnt15 = 0
end function fnt15

