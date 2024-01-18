!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtya00.presh fxdtyia09 cxdtyia09
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
!*      - Testing 2-levels deep derived types with integer components
!*      - Testing FORTRAN functions and C functions
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtyia09
   use ISO_C_BINDING

   type, bind(c) :: dt1
      integer(1) var_a
      integer(C_INT) var_b
      integer(C_SHORT) var_c
      integer(8) var_d
      integer(C_LONG) var_e
      integer(2) var_f
      integer(C_LONG_LONG) var_g
      integer(4) var_h
   end type

   type, bind(c) :: dt2
      integer(C_INT16_T) var_a
      integer(C_INT_FAST8_T) var_b
      integer(C_INT64_T) var_c
      integer(C_INT8_T) var_d
      integer(C_INT_FAST32_T) var_e
      integer(C_INT_FAST16_T) var_f
      integer(C_INT32_T) var_g
      integer(C_INT_FAST64_T) var_h
      type(dt1) :: vdt1
   end type

end module mxdtyia09

module auxmod
   use mxdtyia09

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

program fxdtyia09
   use mxdtyia09
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

   type(dt2) :: dt0 = dt2(2,4,6,8,10,12,14,16, &
                      dt1(2,4,6,8,10,12,14,16))

   type(dt2) :: dta, dtb, dtc
   type(dt2), pointer :: dtd
   type(C_PTR) :: dtp

!! Test 1

   dta = dt0

   dtb = fun1(dta)

   if ( dta%var_a /= 3 .or. dtb%var_a /= 3 ) error stop 20
   if ( dta%var_b /= 5 .or. dtb%var_b /= 5 ) error stop 22
   if ( dta%var_c /= 7 .or. dtb%var_c /= 7 ) error stop 24
   if ( dta%var_d /= 9 .or. dtb%var_d /= 9 ) error stop 26
   if ( dta%var_e /= 11 .or. dtb%var_e /= 11 ) error stop 28
   if ( dta%var_f /= 13 .or. dtb%var_f /= 13 ) error stop 30
   if ( dta%var_g /= 15 .or. dtb%var_g /= 15 ) error stop 32
   if ( dta%var_h /= 17 .or. dtb%var_h /= 17 ) error stop 34

   if ( dta%vdt1%var_a /= 4 .or. dtb%vdt1%var_a /= 4 ) error stop 36
   if ( dta%vdt1%var_b /= 6 .or. dtb%vdt1%var_b /= 6 ) error stop 38
   if ( dta%vdt1%var_c /= 8 .or. dtb%vdt1%var_c /= 8 ) error stop 40
   if ( dta%vdt1%var_d /= 10 .or. dtb%vdt1%var_d /= 10 ) error stop 42
   if ( dta%vdt1%var_e /= 12 .or. dtb%vdt1%var_e /= 12 ) error stop 44
   if ( dta%vdt1%var_f /= 14 .or. dtb%vdt1%var_f /= 14 ) error stop 46
   if ( dta%vdt1%var_g /= 16 .or. dtb%vdt1%var_g /= 16 ) error stop 48
   if ( dta%vdt1%var_h /= 18 .or. dtb%vdt1%var_h /= 18 ) error stop 50

!! Test 2

   dta = dt0

   dtb = fun2(dta)

   if ( dta%var_a /= 2 .or. dtb%var_a /= 3 ) error stop 52
   if ( dta%var_b /= 4 .or. dtb%var_b /= 5 ) error stop 54
   if ( dta%var_c /= 6 .or. dtb%var_c /= 7 ) error stop 56
   if ( dta%var_d /= 8 .or. dtb%var_d /= 9 ) error stop 58
   if ( dta%var_e /= 10 .or. dtb%var_e /= 11 ) error stop 60
   if ( dta%var_f /= 12 .or. dtb%var_f /= 13 ) error stop 62
   if ( dta%var_g /= 14 .or. dtb%var_g /= 15 ) error stop 64
   if ( dta%var_h /= 16 .or. dtb%var_h /= 17 ) error stop 66

   if ( dta%vdt1%var_a /= 2 .or. dtb%vdt1%var_a /= 4 ) error stop 68
   if ( dta%vdt1%var_b /= 4 .or. dtb%vdt1%var_b /= 6 ) error stop 70
   if ( dta%vdt1%var_c /= 6 .or. dtb%vdt1%var_c /= 8 ) error stop 72
   if ( dta%vdt1%var_d /= 8 .or. dtb%vdt1%var_d /= 10 ) error stop 74
   if ( dta%vdt1%var_e /= 10 .or. dtb%vdt1%var_e /= 12 ) error stop 76
   if ( dta%vdt1%var_f /= 12 .or. dtb%vdt1%var_f /= 14 ) error stop 78
   if ( dta%vdt1%var_g /= 14 .or. dtb%vdt1%var_g /= 16 ) error stop 80
   if ( dta%vdt1%var_h /= 16 .or. dtb%vdt1%var_h /= 18 ) error stop 82

!! Test 3

   dta = dt0

   dtc = fun3(dta+dta,dtb)

   if ( dtb%var_a /= 5 .or. dtc%var_a /= 5 ) error stop 84
   if ( dtb%var_b /= 9 .or. dtc%var_b /= 9 ) error stop 86
   if ( dtb%var_c /= 13 .or. dtc%var_c /= 13 ) error stop 88
   if ( dtb%var_d /= 17 .or. dtc%var_d /= 17 ) error stop 90
   if ( dtb%var_e /= 21 .or. dtc%var_e /= 21 ) error stop 92
   if ( dtb%var_f /= 25 .or. dtc%var_f /= 25 ) error stop 94
   if ( dtb%var_g /= 29 .or. dtc%var_g /= 29 ) error stop 96
   if ( dtb%var_h /= 33 .or. dtc%var_h /= 33 ) error stop 98

   if ( dtb%vdt1%var_a /= 6 .or. dtc%vdt1%var_a /= 6 ) error stop 100
   if ( dtb%vdt1%var_b /= 10 .or. dtc%vdt1%var_b /= 10 ) error stop 102
   if ( dtb%vdt1%var_c /= 14 .or. dtc%vdt1%var_c /= 14 ) error stop 104
   if ( dtb%vdt1%var_d /= 18 .or. dtc%vdt1%var_d /= 18 ) error stop 106
   if ( dtb%vdt1%var_e /= 22 .or. dtc%vdt1%var_e /= 22 ) error stop 108
   if ( dtb%vdt1%var_f /= 26 .or. dtc%vdt1%var_f /= 26 ) error stop 110
   if ( dtb%vdt1%var_g /= 30 .or. dtc%vdt1%var_g /= 30 ) error stop 112
   if ( dtb%vdt1%var_h /= 34 .or. dtc%vdt1%var_h /= 34 ) error stop 114

!! Test 4

   dta = dt0

   dtp = fun4(dta+dta,dtb)

   call C_F_POINTER(dtp,dtd)

   if ( dtb%var_a /= 5 .or. dtd%var_a /= 5 ) error stop 116
   if ( dtb%var_b /= 9 .or. dtd%var_b /= 9 ) error stop 118
   if ( dtb%var_c /= 13 .or. dtd%var_c /= 13 ) error stop 120
   if ( dtb%var_d /= 17 .or. dtd%var_d /= 17 ) error stop 122
   if ( dtb%var_e /= 21 .or. dtd%var_e /= 21 ) error stop 124
   if ( dtb%var_f /= 25 .or. dtd%var_f /= 25 ) error stop 126
   if ( dtb%var_g /= 29 .or. dtd%var_g /= 29 ) error stop 128
   if ( dtb%var_h /= 33 .or. dtd%var_h /= 33 ) error stop 130

   if ( dtb%vdt1%var_a /= 6 .or. dtd%vdt1%var_a /= 6 ) error stop 132
   if ( dtb%vdt1%var_b /= 10 .or. dtd%vdt1%var_b /= 10 ) error stop 134
   if ( dtb%vdt1%var_c /= 14 .or. dtd%vdt1%var_c /= 14 ) error stop 136
   if ( dtb%vdt1%var_d /= 18 .or. dtd%vdt1%var_d /= 18 ) error stop 138
   if ( dtb%vdt1%var_e /= 22 .or. dtd%vdt1%var_e /= 22 ) error stop 140
   if ( dtb%vdt1%var_f /= 26 .or. dtd%vdt1%var_f /= 26 ) error stop 142
   if ( dtb%vdt1%var_g /= 30 .or. dtd%vdt1%var_g /= 30 ) error stop 144
   if ( dtb%vdt1%var_h /= 34 .or. dtd%vdt1%var_h /= 34 ) error stop 146

end program fxdtyia09
