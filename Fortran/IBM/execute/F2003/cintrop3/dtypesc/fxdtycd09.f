!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 4/23/2002
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived types with BIND(C) attribute
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - Testing 2-levels deep derived types with BIND(C) attribute
!*      - Testing 2-levels deep derived types with VALUE, INTENT attributes
!*      - Testing 2-levels deep derived types with integer and real components
!*      - Testing FORTRAN functions and C functions
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtycd09
   use ISO_C_BINDING

   type, bind(c) :: dt1
      integer(C_INT_FAST16_T) var_a
      complex(C_DOUBLE_COMPLEX) var_b
      integer(C_SIGNED_CHAR) var_c
      complex(C_FLOAT_COMPLEX) var_d
      complex(16) var_e
      complex(8) var_f
      integer(C_INTMAX_T) var_g
      complex(4) var_h
   end type

   type, bind(c) :: dt2
      complex(C_FLOAT_COMPLEX) var_a
      integer(C_SHORT) var_b
      complex(16) var_c
      complex(8) var_d
      complex(4) var_e
      integer(1) var_f
      complex(C_DOUBLE_COMPLEX) var_g
      integer(4) var_h
      type(dt1) :: vdt1
   end type

end module mxdtycd09

module auxmod
   use mxdtycd09

   interface operator(+)
      module procedure adddty_s, adddty_d
   end interface

contains
   function adddty_s(dtx,dty)
      type(dt1), intent(in) :: dtx, dty
      type(dt1) :: adddty_s

      adddty_s%var_a = dtx%var_a + dty%var_a
      adddty_s%var_b = dtx%var_b + dty%var_b
      adddty_s%var_c = dtx%var_c + dty%var_c
      adddty_s%var_d = dtx%var_d + dty%var_d
      adddty_s%var_e = dtx%var_e + dty%var_e
      adddty_s%var_f = dtx%var_f + dty%var_f
      adddty_s%var_g = dtx%var_g + dty%var_g
      adddty_s%var_h = dtx%var_h + dty%var_h

   end function adddty_s

   function adddty_d(dtx,dty)
      type(dt2), intent(in) :: dtx, dty
      type(dt2) :: adddty_d

      adddty_d%var_a = dtx%var_a + dty%var_a
      adddty_d%var_b = dtx%var_b + dty%var_b
      adddty_d%var_c = dtx%var_c + dty%var_c
      adddty_d%var_d = dtx%var_d + dty%var_d
      adddty_d%var_e = dtx%var_e + dty%var_e
      adddty_d%var_f = dtx%var_f + dty%var_f
      adddty_d%var_g = dtx%var_g + dty%var_g
      adddty_d%var_h = dtx%var_h + dty%var_h

      adddty_d%vdt1 = adddty_s(dtx%vdt1,dty%vdt1)

   end function adddty_d

end module auxmod

program fxdtycd09
   use mxdtycd09
   use auxmod
   interface
      function fun1(dt) bind(c)
         import dt2
         type(dt2) :: fun1, dt
      end function fun1
      function fun2(dt) bind(c)
         import dt2
         type(dt2), value :: dt
         type(dt2) :: fun2
      end function fun2
      function fun3(dtx,dty) bind(c)
         import dt2
         type(dt2), intent(in) :: dtx
         type(dt2), intent(out) :: dty
         type(dt2) :: fun3
      end function fun3
      function fun4(dtx,dty) bind(c)
         import dt2, C_PTR
         type(dt2), intent(in) :: dtx
         type(dt2), intent(out) :: dty
         type(C_PTR) :: fun4
      end function fun4
   end interface

   type(dt2) :: dt0 = dt2((2.0e0,2.0e0),4,(6.0q0,6.0q0),(8.0d0,8.0d0),(10.0e0,10.0e0),12,(14.0d0,14.0d0),16, &
                      dt1(2,(4.0d0,4.0d0),6,(8.0e0,8.0e0),(10.0q0,10.0q0),(12.0d0,12.0d0),14,(16.0e0,16.0e0)))

   type(dt2) :: dta, dtb, dtc
   type(dt2), pointer :: dtd
   type(C_PTR) :: dtp

!! Test 1

   dta = dt0

   dtb = fun1(dta)

   if ( dta%var_a /= (3.0e0,3.0e0) .or. dtb%var_a /= (3.0e0,3.0e0) ) error stop 20
   if ( dta%var_b /= 5 .or. dtb%var_b /= 5 ) error stop 22
   if ( dta%var_c /= (7.0q0,7.0q0) .or. dtb%var_c /= (7.0q0,7.0q0) ) error stop 24
   if ( dta%var_d /= (9.0d0,9.0d0) .or. dtb%var_d /= (9.0d0,9.0d0) ) error stop 26
   if ( dta%var_e /= (11.0e0,11.0e0) .or. dtb%var_e /= (11.0e0,11.0e0) ) error stop 28
   if ( dta%var_f /= 13 .or. dtb%var_f /= 13 ) error stop 30
   if ( dta%var_g /= (15.0d0,15.0d0) .or. dtb%var_g /= (15.0d0,15.0d0) ) error stop 32
   if ( dta%var_h /= 17 .or. dtb%var_h /= 17 ) error stop 34

   if ( dta%vdt1%var_a /= 4 .or. dtb%vdt1%var_a /= 4 ) error stop 36
   if ( dta%vdt1%var_b /= (6.0d0,6.0d0) .or. dtb%vdt1%var_b /= (6.0d0,6.0d0) ) error stop 38
   if ( dta%vdt1%var_c /= 8 .or. dtb%vdt1%var_c /= 8 ) error stop 40
   if ( dta%vdt1%var_d /= (10.0e0,10.0e0) .or. dtb%vdt1%var_d /= (10.0e0,10.0e0) ) error stop 42
   if ( dta%vdt1%var_e /= (12.0q0,12.0q0) .or. dtb%vdt1%var_e /= (12.0q0,12.0q0) ) error stop 44
   if ( dta%vdt1%var_f /= (14.0d0,14.0d0) .or. dtb%vdt1%var_f /= (14.0d0,14.0d0) ) error stop 46
   if ( dta%vdt1%var_g /= 16 .or. dtb%vdt1%var_g /= 16 ) error stop 48
   if ( dta%vdt1%var_h /= (18.0e0,18.0e0) .or. dtb%vdt1%var_h /= (18.0e0,18.0e0) ) error stop 50

!! Test 2

   dta = dt0

   dtb = fun2(dta)

   if ( dta%var_a /= (2.0e0,2.0e0) .or. dtb%var_a /= (3.0e0,3.0e0) ) error stop 52
   if ( dta%var_b /= 4 .or. dtb%var_b /= 5 ) error stop 54
   if ( dta%var_c /= (6.0q0,6.0q0) .or. dtb%var_c /= (7.0q0,7.0q0) ) error stop 56
   if ( dta%var_d /= (8.0d0,8.0d0) .or. dtb%var_d /= (9.0d0,9.0d0) ) error stop 58
   if ( dta%var_e /= (10.0e0,10.0e0) .or. dtb%var_e /= (11.0e0,11.0e0) ) error stop 60
   if ( dta%var_f /= 12 .or. dtb%var_f /= 13 ) error stop 62
   if ( dta%var_g /= (14.0d0,14.0d0) .or. dtb%var_g /= (15.0d0,15.0d0) ) error stop 64
   if ( dta%var_h /= 16 .or. dtb%var_h /= 17 ) error stop 66

   if ( dta%vdt1%var_a /= 2 .or. dtb%vdt1%var_a /= 4 ) error stop 68
   if ( dta%vdt1%var_b /= (4.0d0,4.0d0) .or. dtb%vdt1%var_b /= (6.0d0,6.0d0) ) error stop 70
   if ( dta%vdt1%var_c /= 6 .or. dtb%vdt1%var_c /= 8 ) error stop 72
   if ( dta%vdt1%var_d /= (8.0e0,8.0e0) .or. dtb%vdt1%var_d /= (10.0e0,10.0e0) ) error stop 74
   if ( dta%vdt1%var_e /= (10.0q0,10.0q0) .or. dtb%vdt1%var_e /= (12.0q0,12.0q0) ) error stop 76
   if ( dta%vdt1%var_f /= (12.0d0,12.0d0) .or. dtb%vdt1%var_f /= (14.0d0,14.0d0) ) error stop 78
   if ( dta%vdt1%var_g /= 14 .or. dtb%vdt1%var_g /= 16 ) error stop 80
   if ( dta%vdt1%var_h /= (16.0e0,16.0e0) .or. dtb%vdt1%var_h /= (18.0e0,18.0e0) ) error stop 82

!! Test 3

   dta = dt0

   dtc = fun3(dta+dta,dtb)

   if ( dtb%var_a /= (5.0e0,5.0e0) .or. dtc%var_a /= (5.0e0,5.0e0) ) error stop 84
   if ( dtb%var_b /= 9 .or. dtc%var_b /= 9 ) error stop 86
   if ( dtb%var_c /= (13.0q0,13.0q0) .or. dtc%var_c /= (13.0q0,13.0q0) ) error stop 88
   if ( dtb%var_d /= (17.0d0,17.0d0) .or. dtc%var_d /= (17.0d0,17.0d0) ) error stop 90
   if ( dtb%var_e /= (21.0e0,21.0e0) .or. dtc%var_e /= (21.0e0,21.0e0) ) error stop 92
   if ( dtb%var_f /= 25 .or. dtc%var_f /= 25 ) error stop 94
   if ( dtb%var_g /= (29.0d0,29.0d0) .or. dtc%var_g /= (29.0d0,29.0d0) ) error stop 96
   if ( dtb%var_h /= 33 .or. dtc%var_h /= 33 ) error stop 98

   if ( dtb%vdt1%var_a /= 6 .or. dtc%vdt1%var_a /= 6 ) error stop 100
   if ( dtb%vdt1%var_b /= (10.0d0,10.0d0) .or. dtc%vdt1%var_b /= (10.0d0,10.0d0) ) error stop 102
   if ( dtb%vdt1%var_c /= 14 .or. dtc%vdt1%var_c /= 14 ) error stop 104
   if ( dtb%vdt1%var_d /= (18.0e0,18.0e0) .or. dtc%vdt1%var_d /= (18.0e0,18.0e0) ) error stop 106
   if ( dtb%vdt1%var_e /= (22.0q0,22.0q0) .or. dtc%vdt1%var_e /= (22.0q0,22.0q0) ) error stop 108
   if ( dtb%vdt1%var_f /= (26.0d0,26.0d0) .or. dtc%vdt1%var_f /= (26.0d0,26.0d0) ) error stop 110
   if ( dtb%vdt1%var_g /= 30 .or. dtc%vdt1%var_g /= 30 ) error stop 112
   if ( dtb%vdt1%var_h /= (34.0e0,34.0e0) .or. dtc%vdt1%var_h /= (34.0e0,34.0e0) ) error stop 114

!! Test 4

   dta = dt0

   dtp = fun4(dta+dta,dtb)

   call C_F_POINTER(dtp,dtd)

   if ( dtb%var_a /= (5.0e0,5.0e0) .or. dtd%var_a /= (5.0e0,5.0e0) ) error stop 116
   if ( dtb%var_b /= 9 .or. dtd%var_b /= 9 ) error stop 118
   if ( dtb%var_c /= (13.0q0,13.0q0) .or. dtd%var_c /= (13.0q0,13.0q0) ) error stop 120
   if ( dtb%var_d /= (17.0d0,17.0d0) .or. dtd%var_d /= (17.0d0,17.0d0) ) error stop 122
   if ( dtb%var_e /= (21.0e0,21.0e0) .or. dtd%var_e /= (21.0e0,21.0e0) ) error stop 124
   if ( dtb%var_f /= 25 .or. dtd%var_f /= 25 ) error stop 126
   if ( dtb%var_g /= (29.0d0,29.0d0) .or. dtd%var_g /= (29.0d0,29.0d0) ) error stop 128
   if ( dtb%var_h /= 33 .or. dtd%var_h /= 33 ) error stop 130

   if ( dtb%vdt1%var_a /= 6 .or. dtd%vdt1%var_a /= 6 ) error stop 132
   if ( dtb%vdt1%var_b /= (10.0d0,10.0d0) .or. dtd%vdt1%var_b /= (10.0d0,10.0d0) ) error stop 134
   if ( dtb%vdt1%var_c /= 14 .or. dtd%vdt1%var_c /= 14 ) error stop 136
   if ( dtb%vdt1%var_d /= (18.0e0,18.0e0) .or. dtd%vdt1%var_d /= (18.0e0,18.0e0) ) error stop 138
   if ( dtb%vdt1%var_e /= (22.0q0,22.0q0) .or. dtd%vdt1%var_e /= (22.0q0,22.0q0) ) error stop 140
   if ( dtb%vdt1%var_f /= (26.0d0,26.0d0) .or. dtd%vdt1%var_f /= (26.0d0,26.0d0) ) error stop 142
   if ( dtb%vdt1%var_g /= 30 .or. dtd%vdt1%var_g /= 30 ) error stop 144
   if ( dtb%vdt1%var_h /= (34.0e0,34.0e0) .or. dtd%vdt1%var_h /= (34.0e0,34.0e0) ) error stop 146

end program fxdtycd09
