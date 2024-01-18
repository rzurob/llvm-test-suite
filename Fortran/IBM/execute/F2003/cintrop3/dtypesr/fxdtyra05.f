!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtya00.presh fxdtyra05 cxdtyra05
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
!*  TEST CASE TITLE            : Support for derived types with BIND(C) attr.
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquide
!*  DATE                       : 4/23/2002
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived types with BIND(C) attribute
!*  SECONDARY FUNCTIONS TESTED : see below 
!*
!*  DRIVER STANZA              : 
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - Testing 3-levels deep derived types with BIND(C) attribute
!*      - Testing 3-levels deep derived types with VALUE, INTENT attributes
!*      - Testing 3-levels deep derived types with integer and real components
!*      - Testing FORTRAN functions and C functions
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtyra05
   use ISO_C_BINDING

   type, bind(c) :: dt1
      integer(C_INT_FAST16_T) var_a
      real(C_DOUBLE) var_b
      integer(C_SIGNED_CHAR) var_c
      real(C_FLOAT) var_d
      real(C_LONG_DOUBLE) var_e
      real(8) var_f
      integer(C_INTMAX_T) var_g
      real(4) var_h
   end type

   type, bind(c) :: dt2
      real(C_DOUBLE) var_a
      integer(C_INT) var_b
      real(8) var_c
      integer(1) var_d
      real(C_LONG_DOUBLE) var_e
      real(4) var_f
      integer(C_INT32_T) var_g
      real(C_FLOAT) var_h
      type(dt1) :: vdt1
   end type

   type, bind(c) :: dt3
      type(dt2) :: vdt2
      real(C_FLOAT) var_a
      integer(C_SHORT) var_b
      real(C_LONG_DOUBLE) var_c
      real(8) var_d
      real(4) var_e
      integer(1) var_f
      real(C_DOUBLE) var_g
      integer(4) var_h
   end type

end module mxdtyra05

module auxmod
   use mxdtyra05

   interface operator(+)
      module procedure adddty_s, adddty_d, adddty_t
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

   function adddty_t(dtx,dty)
      type(dt3), intent(in) :: dtx, dty
      type(dt3) :: adddty_t

      adddty_t%var_a = dtx%var_a + dty%var_a
      adddty_t%var_b = dtx%var_b + dty%var_b
      adddty_t%var_c = dtx%var_c + dty%var_c
      adddty_t%var_d = dtx%var_d + dty%var_d
      adddty_t%var_e = dtx%var_e + dty%var_e
      adddty_t%var_f = dtx%var_f + dty%var_f
      adddty_t%var_g = dtx%var_g + dty%var_g
      adddty_t%var_h = dtx%var_h + dty%var_h

      adddty_t%vdt2 = adddty_d(dtx%vdt2,dty%vdt2)

   end function adddty_t

end module auxmod

program fxdtyra05
   use mxdtyra05
   use auxmod
   interface
      function fun1(dt) bind(c)
         import dt3
         type(dt3) :: fun1, dt
      end function fun1
      function fun2(dt) bind(c)
         import dt3
         type(dt3), value :: dt
         type(dt3) :: fun2
      end function fun2
      function fun3(dtx,dty) bind(c)
         import dt3
         type(dt3), intent(in) :: dtx
         type(dt3), intent(out) :: dty
         type(dt3) :: fun3
      end function fun3
      function fun4(dtx,dty) bind(c)
         import dt3, C_PTR
         type(dt3), intent(in) :: dtx
         type(dt3), intent(out) :: dty
         type(C_PTR) :: fun4
      end function fun4
   end interface

   type(dt3) :: dt0 = dt3(dt2(2.0d0,4,6.0d0,8,10.0q0,12.0e0,14,16.0e0, &
                      dt1(2,4.0d0,6,8.0e0,10.0q0,12.0d0,14,16.0e0)), &
                      2.0e0,4,6.0q0,8.0d0,10.0e0,12,14.0d0,16)

   type(dt3) :: dta, dtb, dtc
   type(dt3), pointer :: dtd
   type(C_PTR) :: dtp

!! Test 1

   dta = dt0

   dtb = fun1(dta)

   if ( dta%var_a /= 3.0e0 .or. dtb%var_a /= 3.0e0 ) error stop 20
   if ( dta%var_b /= 5 .or. dtb%var_b /= 5 ) error stop 22
   if ( dta%var_c /= 7.0q0 .or. dtb%var_c /= 7.0q0 ) error stop 24
   if ( dta%var_d /= 9.0d0 .or. dtb%var_d /= 9.0d0 ) error stop 26
   if ( dta%var_e /= 11.0e0 .or. dtb%var_e /= 11.0e0 ) error stop 28
   if ( dta%var_f /= 13 .or. dtb%var_f /= 13 ) error stop 30
   if ( dta%var_g /= 15.0d0 .or. dtb%var_g /= 15.0d0 ) error stop 32
   if ( dta%var_h /= 17 .or. dtb%var_h /= 17 ) error stop 34

   if ( dta%vdt2%var_a /= 4.0d0 .or. dtb%vdt2%var_a /= 4.0d0 ) error stop 36
   if ( dta%vdt2%var_b /= 6 .or. dtb%vdt2%var_b /= 6 ) error stop 38
   if ( dta%vdt2%var_c /= 8.0d0 .or. dtb%vdt2%var_c /= 8.0d0 ) error stop 40
   if ( dta%vdt2%var_d /= 10 .or. dtb%vdt2%var_d /= 10 ) error stop 42
   if ( dta%vdt2%var_e /= 12.0q0 .or. dtb%vdt2%var_e /= 12.0q0 ) error stop 44
   if ( dta%vdt2%var_f /= 14.0e0 .or. dtb%vdt2%var_f /= 14.0e0 ) error stop 46
   if ( dta%vdt2%var_g /= 16 .or. dtb%vdt2%var_g /= 16 ) error stop 48
   if ( dta%vdt2%var_h /= 18.0e0 .or. dtb%vdt2%var_h /= 18.0e0 ) error stop 50

   if ( dta%vdt2%vdt1%var_a /= 5 .or. &
                               dtb%vdt2%vdt1%var_a /= 5 ) error stop 52
   if ( dta%vdt2%vdt1%var_b /= 7.0d0 .or. &
                               dtb%vdt2%vdt1%var_b /= 7.0d0 ) error stop 54
   if ( dta%vdt2%vdt1%var_c /= 9 .or. &
                               dtb%vdt2%vdt1%var_c /= 9 ) error stop 56
   if ( dta%vdt2%vdt1%var_d /= 11.0e0 .or. &
                               dtb%vdt2%vdt1%var_d /= 11.0e0 ) error stop 58
   if ( dta%vdt2%vdt1%var_e /= 13.0q0 .or. &
                               dtb%vdt2%vdt1%var_e /= 13.0q0 ) error stop 60
   if ( dta%vdt2%vdt1%var_f /= 15.0d0 .or. &
                               dtb%vdt2%vdt1%var_f /= 15.0d0 ) error stop 62
   if ( dta%vdt2%vdt1%var_g /= 17 .or. &
                               dtb%vdt2%vdt1%var_g /= 17 ) error stop 64
   if ( dta%vdt2%vdt1%var_h /= 19.0e0 .or. &
                               dtb%vdt2%vdt1%var_h /= 19.0e0 ) error stop 66

!! Test 2

   dta = dt0

   dtb = fun2(dta)

   if ( dta%var_a /= 2.0e0 .or. dtb%var_a /= 3.0e0 ) error stop 68
   if ( dta%var_b /= 4 .or. dtb%var_b /= 5 ) error stop 70
   if ( dta%var_c /= 6.0q0 .or. dtb%var_c /= 7.0q0 ) error stop 72
   if ( dta%var_d /= 8.0d0 .or. dtb%var_d /= 9.0d0 ) error stop 74
   if ( dta%var_e /= 10.0e0 .or. dtb%var_e /= 11.0e0 ) error stop 76
   if ( dta%var_f /= 12 .or. dtb%var_f /= 13 ) error stop 78
   if ( dta%var_g /= 14.0d0 .or. dtb%var_g /= 15.0d0 ) error stop 80
   if ( dta%var_h /= 16 .or. dtb%var_h /= 17 ) error stop 82

   if ( dta%vdt2%var_a /= 2.0d0 .or. dtb%vdt2%var_a /= 4.0d0 ) error stop 84
   if ( dta%vdt2%var_b /= 4 .or. dtb%vdt2%var_b /= 6 ) error stop 86
   if ( dta%vdt2%var_c /= 6.0d0 .or. dtb%vdt2%var_c /= 8.0d0 ) error stop 88
   if ( dta%vdt2%var_d /= 8 .or. dtb%vdt2%var_d /= 10 ) error stop 90
   if ( dta%vdt2%var_e /= 10.0q0 .or. dtb%vdt2%var_e /= 12.0q0 ) error stop 92
   if ( dta%vdt2%var_f /= 12.0e0 .or. dtb%vdt2%var_f /= 14.0e0 ) error stop 94
   if ( dta%vdt2%var_g /= 14 .or. dtb%vdt2%var_g /= 16 ) error stop 96
   if ( dta%vdt2%var_h /= 16.0e0 .or. dtb%vdt2%var_h /= 18.0e0 ) error stop 98

   if ( dta%vdt2%vdt1%var_a /= 2 .or. &
                               dtb%vdt2%vdt1%var_a /= 5 ) error stop 100
   if ( dta%vdt2%vdt1%var_b /= 4.0d0 .or. &
                               dtb%vdt2%vdt1%var_b /= 7.0d0 ) error stop 102
   if ( dta%vdt2%vdt1%var_c /= 6 .or. &
                               dtb%vdt2%vdt1%var_c /= 9 ) error stop 104
   if ( dta%vdt2%vdt1%var_d /= 8.0e0 .or. &
                               dtb%vdt2%vdt1%var_d /= 11.0e0 ) error stop 106
   if ( dta%vdt2%vdt1%var_e /= 10.0q0 .or. &
                               dtb%vdt2%vdt1%var_e /= 13.0q0 ) error stop 108
   if ( dta%vdt2%vdt1%var_f /= 12.0d0 .or. &
                               dtb%vdt2%vdt1%var_f /= 15.0d0 ) error stop 110
   if ( dta%vdt2%vdt1%var_g /= 14 .or. &
                               dtb%vdt2%vdt1%var_g /= 17 ) error stop 112
   if ( dta%vdt2%vdt1%var_h /= 16.0e0 .or. &
                               dtb%vdt2%vdt1%var_h /= 19.0e0 ) error stop 114

!! Test 3

   dta = dt0

   dtc = fun3(dta+dta,dtb)

   if ( dtb%var_a /= 5.0e0 .or. dtc%var_a /= 5.0e0 ) error stop 116
   if ( dtb%var_b /= 9 .or. dtc%var_b /= 9 ) error stop 118
   if ( dtb%var_c /= 13.0q0 .or. dtc%var_c /= 13.0q0 ) error stop 120
   if ( dtb%var_d /= 17.0d0 .or. dtc%var_d /= 17.0d0 ) error stop 122
   if ( dtb%var_e /= 21.0e0 .or. dtc%var_e /= 21.0e0 ) error stop 124
   if ( dtb%var_f /= 25 .or. dtc%var_f /= 25 ) error stop 126
   if ( dtb%var_g /= 29.0d0 .or. dtc%var_g /= 29.0d0 ) error stop 128
   if ( dtb%var_h /= 33 .or. dtc%var_h /= 33 ) error stop 130

   if ( dtb%vdt2%var_a /= 6.0d0 .or. dtc%vdt2%var_a /= 6.0d0 ) error stop 132
   if ( dtb%vdt2%var_b /= 10 .or. dtc%vdt2%var_b /= 10 ) error stop 134
   if ( dtb%vdt2%var_c /= 14.0d0 .or. dtc%vdt2%var_c /= 14.0d0 ) error stop 136
   if ( dtb%vdt2%var_d /= 18 .or. dtc%vdt2%var_d /= 18 ) error stop 138
   if ( dtb%vdt2%var_e /= 22.0q0 .or. dtc%vdt2%var_e /= 22.0q0 ) error stop 140
   if ( dtb%vdt2%var_f /= 26.0e0 .or. dtc%vdt2%var_f /= 26.0e0 ) error stop 142
   if ( dtb%vdt2%var_g /= 30 .or. dtc%vdt2%var_g /= 30 ) error stop 144
   if ( dtb%vdt2%var_h /= 34.0e0 .or. dtc%vdt2%var_h /= 34.0e0 ) error stop 146

   if ( dtb%vdt2%vdt1%var_a /= 7 .or. &
                              dtc%vdt2%vdt1%var_a /= 7 ) error stop 148
   if ( dtb%vdt2%vdt1%var_b /= 11.0d0 .or. &
                              dtc%vdt2%vdt1%var_b /= 11.0d0 ) error stop 150
   if ( dtb%vdt2%vdt1%var_c /= 15 .or. &
                              dtc%vdt2%vdt1%var_c /= 15 ) error stop 152
   if ( dtb%vdt2%vdt1%var_d /= 19.0e0 .or. &
                              dtc%vdt2%vdt1%var_d /= 19.0e0 ) error stop 154
   if ( dtb%vdt2%vdt1%var_e /= 23.0q0 .or. &
                              dtc%vdt2%vdt1%var_e /= 23.0q0 ) error stop 156
   if ( dtb%vdt2%vdt1%var_f /= 27.0d0 .or. &
                              dtc%vdt2%vdt1%var_f /= 27.0d0 ) error stop 158
   if ( dtb%vdt2%vdt1%var_g /= 31 .or. &
                              dtc%vdt2%vdt1%var_g /= 31 ) error stop 160
   if ( dtb%vdt2%vdt1%var_h /= 35.0e0 .or. &
                              dtc%vdt2%vdt1%var_h /= 35.0e0 ) error stop 162

!! Test 4

   dta = dt0

   dtp = fun4(dta+dta,dtb)

   call C_F_POINTER(dtp,dtd)

   if ( dtb%var_a /= 5.0e0 .or. dtd%var_a /= 5.0e0 ) error stop 164
   if ( dtb%var_b /= 9 .or. dtd%var_b /= 9 ) error stop 166
   if ( dtb%var_c /= 13.0q0 .or. dtd%var_c /= 13.0q0 ) error stop 168
   if ( dtb%var_d /= 17.0d0 .or. dtd%var_d /= 17.0d0 ) error stop 170
   if ( dtb%var_e /= 21.0e0 .or. dtd%var_e /= 21.0e0 ) error stop 172
   if ( dtb%var_f /= 25 .or. dtd%var_f /= 25 ) error stop 174
   if ( dtb%var_g /= 29.0d0 .or. dtd%var_g /= 29.0d0 ) error stop 176
   if ( dtb%var_h /= 33 .or. dtd%var_h /= 33 ) error stop 178

   if ( dtb%vdt2%var_a /= 6.0d0 .or. dtd%vdt2%var_a /= 6.0d0 ) error stop 180
   if ( dtb%vdt2%var_b /= 10 .or. dtd%vdt2%var_b /= 10 ) error stop 182
   if ( dtb%vdt2%var_c /= 14.0d0 .or. dtd%vdt2%var_c /= 14.0d0 ) error stop 184
   if ( dtb%vdt2%var_d /= 18 .or. dtd%vdt2%var_d /= 18 ) error stop 186
   if ( dtb%vdt2%var_e /= 22.0q0 .or. dtd%vdt2%var_e /= 22.0q0 ) error stop 188
   if ( dtb%vdt2%var_f /= 26.0e0 .or. dtd%vdt2%var_f /= 26.0e0 ) error stop 190
   if ( dtb%vdt2%var_g /= 30 .or. dtd%vdt2%var_g /= 30 ) error stop 192
   if ( dtb%vdt2%var_h /= 34.0e0 .or. dtd%vdt2%var_h /= 34.0e0 ) error stop 194

   if ( dtb%vdt2%vdt1%var_a /= 7 .or. &
                              dtd%vdt2%vdt1%var_a /= 7 ) error stop 196
   if ( dtb%vdt2%vdt1%var_b /= 11.0d0 .or. &
                              dtd%vdt2%vdt1%var_b /= 11.0d0 ) error stop 198
   if ( dtb%vdt2%vdt1%var_c /= 15 .or. &
                              dtd%vdt2%vdt1%var_c /= 15 ) error stop 200
   if ( dtb%vdt2%vdt1%var_d /= 19.0e0 .or. &
                              dtd%vdt2%vdt1%var_d /= 19.0e0 ) error stop 202
   if ( dtb%vdt2%vdt1%var_e /= 23.0q0 .or. &
                              dtd%vdt2%vdt1%var_e /= 23.0q0 ) error stop 204
   if ( dtb%vdt2%vdt1%var_f /= 27.0d0 .or. &
                              dtd%vdt2%vdt1%var_f /= 27.0d0 ) error stop 206
   if ( dtb%vdt2%vdt1%var_g /= 31 .or. &
                              dtd%vdt2%vdt1%var_g /= 31 ) error stop 208
   if ( dtb%vdt2%vdt1%var_h /= 35.0e0 .or. &
                              dtd%vdt2%vdt1%var_h /= 35.0e0 ) error stop 210

end program fxdtyra05
