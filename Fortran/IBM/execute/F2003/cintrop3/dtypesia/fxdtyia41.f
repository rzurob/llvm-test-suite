!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtya01.presh fxdtyia41
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
!*      - Testing 3-levels deep derived types with integer components
!*      - Testing FORTRAN module functions and host association
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtyia41
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

   type, bind(c) :: dt3
      type(dt2) :: vdt2
      integer(C_SIZE_T) var_a
      integer(C_INT_LEAST8_T) var_b
      integer(C_INT_LEAST64_T) var_c
      integer(C_SIGNED_CHAR) var_d
      integer(C_INTPTR_T) var_e
      integer(C_INT_LEAST16_T) var_f
      integer(C_INTMAX_T) var_g
      integer(C_INT_LEAST32_T) var_h
   end type

   type(dt3) :: dta, dtb, dtc
   type(dt3), pointer :: dtd
   type(C_PTR) :: dtp

contains

function fun1()
   type(dt3) :: fun1

   if ( dta%var_a /= 2 .or. dta%vdt2%var_a /= 2 .or. &
                          dta%vdt2%vdt1%var_a /= 2 ) error stop 20
   if ( dta%var_b /= 4 .or. dta%vdt2%var_b /= 4 .or. &
                          dta%vdt2%vdt1%var_b /= 4 ) error stop 22
   if ( dta%var_c /= 6 .or. dta%vdt2%var_c /= 6 .or. &
                          dta%vdt2%vdt1%var_c /= 6 ) error stop 24
   if ( dta%var_d /= 8 .or. dta%vdt2%var_d /= 8 .or. &
                          dta%vdt2%vdt1%var_d /= 8 ) error stop 26
   if ( dta%var_e /= 10 .or. dta%vdt2%var_e /= 10 .or. &
                           dta%vdt2%vdt1%var_e /= 10 ) error stop 28
   if ( dta%var_f /= 12 .or. dta%vdt2%var_f /= 12 .or. &
                           dta%vdt2%vdt1%var_f /= 12 ) error stop 30
   if ( dta%var_g /= 14 .or. dta%vdt2%var_g /= 14 .or. &
                           dta%vdt2%vdt1%var_h /= 16 ) error stop 32
   if ( dta%var_h /= 16 .or. dta%vdt2%var_h /= 16 .or. &
                           dta%vdt2%vdt1%var_h /= 16 ) error stop 34

   dta%var_a = dta%var_a + 1
   dta%var_b = dta%var_b + 1
   dta%var_c = dta%var_c + 1
   dta%var_d = dta%var_d + 1
   dta%var_e = dta%var_e + 1
   dta%var_f = dta%var_f + 1
   dta%var_g = dta%var_g + 1
   dta%var_h = dta%var_h + 1

   dta%vdt2%var_a = dta%vdt2%var_a + 2
   dta%vdt2%var_b = dta%vdt2%var_b + 2
   dta%vdt2%var_c = dta%vdt2%var_c + 2
   dta%vdt2%var_d = dta%vdt2%var_d + 2
   dta%vdt2%var_e = dta%vdt2%var_e + 2
   dta%vdt2%var_f = dta%vdt2%var_f + 2
   dta%vdt2%var_g = dta%vdt2%var_g + 2
   dta%vdt2%var_h = dta%vdt2%var_h + 2

   dta%vdt2%vdt1%var_a = dta%vdt2%vdt1%var_a + 3
   dta%vdt2%vdt1%var_b = dta%vdt2%vdt1%var_b + 3
   dta%vdt2%vdt1%var_c = dta%vdt2%vdt1%var_c + 3
   dta%vdt2%vdt1%var_d = dta%vdt2%vdt1%var_d + 3
   dta%vdt2%vdt1%var_e = dta%vdt2%vdt1%var_e + 3
   dta%vdt2%vdt1%var_f = dta%vdt2%vdt1%var_f + 3
   dta%vdt2%vdt1%var_g = dta%vdt2%vdt1%var_g + 3
   dta%vdt2%vdt1%var_h = dta%vdt2%vdt1%var_h + 3

   fun1 = dta

end function fun1

function fun3(dtu)
   type(dt3), intent(in) :: dtu
   type(dt3) :: fun3

   if ( dtu%var_a /= 4 .or. dtu%vdt2%var_a /= 4 .or. &
                            dtu%vdt2%vdt1%var_a /= 4 ) error stop 36
   if ( dtu%var_b /= 8 .or. dtu%vdt2%var_b /= 8 .or. & 
                            dtu%vdt2%vdt1%var_b /= 8 ) error stop 38
   if ( dtu%var_c /= 12 .or. dtu%vdt2%var_c /= 12 .or. &
                             dtu%vdt2%vdt1%var_c /= 12 ) error stop 40
   if ( dtu%var_d /= 16 .or. dtu%vdt2%var_d /= 16 .or. &
                             dtu%vdt2%vdt1%var_d /= 16 ) error stop 42
   if ( dtu%var_e /= 20 .or. dtu%vdt2%var_e /= 20 .or. &
                             dtu%vdt2%vdt1%var_e /= 20 ) error stop 44
   if ( dtu%var_f /= 24 .or. dtu%vdt2%var_f /= 24 .or. &
                             dtu%vdt2%vdt1%var_f /= 24 ) error stop 46
   if ( dtu%var_g /= 28 .or. dtu%vdt2%var_g /= 28 .or. &
                             dtu%vdt2%vdt1%var_g /= 28 ) error stop 48
   if ( dtu%var_h /= 32 .or. dtu%vdt2%var_h /= 32 .or. &
                             dtu%vdt2%vdt1%var_h /= 32 ) error stop 50

   dta%var_a = dtu%var_a + 1
   dta%var_b = dtu%var_b + 1
   dta%var_c = dtu%var_c + 1
   dta%var_d = dtu%var_d + 1
   dta%var_e = dtu%var_e + 1
   dta%var_f = dtu%var_f + 1
   dta%var_g = dtu%var_g + 1
   dta%var_h = dtu%var_h + 1

   dta%vdt2%var_a = dtu%vdt2%var_a + 2
   dta%vdt2%var_b = dtu%vdt2%var_b + 2
   dta%vdt2%var_c = dtu%vdt2%var_c + 2
   dta%vdt2%var_d = dtu%vdt2%var_d + 2
   dta%vdt2%var_e = dtu%vdt2%var_e + 2
   dta%vdt2%var_f = dtu%vdt2%var_f + 2
   dta%vdt2%var_g = dtu%vdt2%var_g + 2
   dta%vdt2%var_h = dtu%vdt2%var_h + 2

   dta%vdt2%vdt1%var_a = dtu%vdt2%vdt1%var_a + 3
   dta%vdt2%vdt1%var_b = dtu%vdt2%vdt1%var_b + 3
   dta%vdt2%vdt1%var_c = dtu%vdt2%vdt1%var_c + 3
   dta%vdt2%vdt1%var_d = dtu%vdt2%vdt1%var_d + 3
   dta%vdt2%vdt1%var_e = dtu%vdt2%vdt1%var_e + 3
   dta%vdt2%vdt1%var_f = dtu%vdt2%vdt1%var_f + 3
   dta%vdt2%vdt1%var_g = dtu%vdt2%vdt1%var_g + 3
   dta%vdt2%vdt1%var_h = dtu%vdt2%vdt1%var_h + 3

   fun3 = dta

end function fun3

function fun4(dtu)
   type(dt3), intent(in) :: dtu
   type(dt3), target, save :: dtz
   type(C_PTR) :: fun4

   if ( dtu%var_a /= 4 .or. dtu%vdt2%var_a /= 4 .or. &
                            dtu%vdt2%vdt1%var_a /= 4 ) error stop 52
   if ( dtu%var_b /= 8 .or. dtu%vdt2%var_b /= 8 .or. &
                            dtu%vdt2%vdt1%var_b /= 8 ) error stop 54
   if ( dtu%var_c /= 12 .or. dtu%vdt2%var_c /= 12 .or. &
                             dtu%vdt2%vdt1%var_c /= 12 ) error stop 56
   if ( dtu%var_d /= 16 .or. dtu%vdt2%var_d /= 16 .or. &
                             dtu%vdt2%vdt1%var_d /= 16 ) error stop 58
   if ( dtu%var_e /= 20 .or. dtu%vdt2%var_e /= 20 .or. &
                             dtu%vdt2%vdt1%var_e /= 20 ) error stop 60
   if ( dtu%var_f /= 24 .or. dtu%vdt2%var_f /= 24 .or. &
                             dtu%vdt2%vdt1%var_f /= 24 ) error stop 62
   if ( dtu%var_g /= 28 .or. dtu%vdt2%var_g /= 28 .or. &
                             dtu%vdt2%vdt1%var_g /= 28 ) error stop 64
   if ( dtu%var_h /= 32 .or. dtu%vdt2%var_h /= 32 .or. &
                             dtu%vdt2%vdt1%var_h /= 32 ) error stop 66

   dtz%var_a = dtu%var_a + 1
   dtz%var_b = dtu%var_b + 1
   dtz%var_c = dtu%var_c + 1
   dtz%var_d = dtu%var_d + 1
   dtz%var_e = dtu%var_e + 1
   dtz%var_f = dtu%var_f + 1
   dtz%var_g = dtu%var_g + 1
   dtz%var_h = dtu%var_h + 1

   dtz%vdt2%var_a = dtu%vdt2%var_a + 2
   dtz%vdt2%var_b = dtu%vdt2%var_b + 2
   dtz%vdt2%var_c = dtu%vdt2%var_c + 2
   dtz%vdt2%var_d = dtu%vdt2%var_d + 2
   dtz%vdt2%var_e = dtu%vdt2%var_e + 2
   dtz%vdt2%var_f = dtu%vdt2%var_f + 2
   dtz%vdt2%var_g = dtu%vdt2%var_g + 2
   dtz%vdt2%var_h = dtu%vdt2%var_h + 2

   dtz%vdt2%vdt1%var_a = dtu%vdt2%vdt1%var_a + 3
   dtz%vdt2%vdt1%var_b = dtu%vdt2%vdt1%var_b + 3
   dtz%vdt2%vdt1%var_c = dtu%vdt2%vdt1%var_c + 3
   dtz%vdt2%vdt1%var_d = dtu%vdt2%vdt1%var_d + 3
   dtz%vdt2%vdt1%var_e = dtu%vdt2%vdt1%var_e + 3
   dtz%vdt2%vdt1%var_f = dtu%vdt2%vdt1%var_f + 3
   dtz%vdt2%vdt1%var_g = dtu%vdt2%vdt1%var_g + 3
   dtz%vdt2%vdt1%var_h = dtu%vdt2%vdt1%var_h + 3

   dta = dtz

   fun4 = C_LOC(dtz)

end function fun4

end module mxdtyia41

module auxmod
   use mxdtyia41

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

program fxdtyia41
   use mxdtyia41
   use auxmod

   type(dt3) :: dt0 = dt3(dt2(2,4,6,8,10,12,14,16, &
                      dt1(2,4,6,8,10,12,14,16)), &
                      2,4,6,8,10,12,14,16)

!! Test 1

   dta = dt0

   dtb = fun1()

   if ( dta%var_a /= 3 .or. dtb%var_a /= 3 ) error stop 68
   if ( dta%var_b /= 5 .or. dtb%var_b /= 5 ) error stop 70
   if ( dta%var_c /= 7 .or. dtb%var_c /= 7 ) error stop 72
   if ( dta%var_d /= 9 .or. dtb%var_d /= 9 ) error stop 74
   if ( dta%var_e /= 11 .or. dtb%var_e /= 11 ) error stop 76
   if ( dta%var_f /= 13 .or. dtb%var_f /= 13 ) error stop 78
   if ( dta%var_g /= 15 .or. dtb%var_g /= 15 ) error stop 80
   if ( dta%var_h /= 17 .or. dtb%var_h /= 17 ) error stop 82

   if ( dta%vdt2%var_a /= 4 .or. dtb%vdt2%var_a /= 4 ) error stop 84
   if ( dta%vdt2%var_b /= 6 .or. dtb%vdt2%var_b /= 6 ) error stop 86
   if ( dta%vdt2%var_c /= 8 .or. dtb%vdt2%var_c /= 8 ) error stop 88
   if ( dta%vdt2%var_d /= 10 .or. dtb%vdt2%var_d /= 10 ) error stop 90
   if ( dta%vdt2%var_e /= 12 .or. dtb%vdt2%var_e /= 12 ) error stop 92
   if ( dta%vdt2%var_f /= 14 .or. dtb%vdt2%var_f /= 14 ) error stop 94
   if ( dta%vdt2%var_g /= 16 .or. dtb%vdt2%var_g /= 16 ) error stop 96
   if ( dta%vdt2%var_h /= 18 .or. dtb%vdt2%var_h /= 18 ) error stop 98

   if ( dta%vdt2%vdt1%var_a /= 5 .or. &
                               dtb%vdt2%vdt1%var_a /= 5 ) error stop 100
   if ( dta%vdt2%vdt1%var_b /= 7 .or. &
                               dtb%vdt2%vdt1%var_b /= 7 ) error stop 102
   if ( dta%vdt2%vdt1%var_c /= 9 .or. &
                               dtb%vdt2%vdt1%var_c /= 9 ) error stop 104
   if ( dta%vdt2%vdt1%var_d /= 11 .or. &
                               dtb%vdt2%vdt1%var_d /= 11 ) error stop 106
   if ( dta%vdt2%vdt1%var_e /= 13 .or. &
                               dtb%vdt2%vdt1%var_e /= 13 ) error stop 108
   if ( dta%vdt2%vdt1%var_f /= 15 .or. &
                               dtb%vdt2%vdt1%var_f /= 15 ) error stop 110
   if ( dta%vdt2%vdt1%var_g /= 17 .or. &
                               dtb%vdt2%vdt1%var_g /= 17 ) error stop 112
   if ( dta%vdt2%vdt1%var_h /= 19 .or. &
                               dtb%vdt2%vdt1%var_h /= 19 ) error stop 114

!! Test 3

   dta = dt0

   dtc = fun3(dta+dta)

   if ( dta%var_a /= 5 .or. dtc%var_a /= 5 ) error stop 116
   if ( dta%var_b /= 9 .or. dtc%var_b /= 9 ) error stop 118
   if ( dta%var_c /= 13 .or. dtc%var_c /= 13 ) error stop 120
   if ( dta%var_d /= 17 .or. dtc%var_d /= 17 ) error stop 122
   if ( dta%var_e /= 21 .or. dtc%var_e /= 21 ) error stop 124
   if ( dta%var_f /= 25 .or. dtc%var_f /= 25 ) error stop 126
   if ( dta%var_g /= 29 .or. dtc%var_g /= 29 ) error stop 128
   if ( dta%var_h /= 33 .or. dtc%var_h /= 33 ) error stop 130

   if ( dta%vdt2%var_a /= 6 .or. dtc%vdt2%var_a /= 6 ) error stop 132
   if ( dta%vdt2%var_b /= 10 .or. dtc%vdt2%var_b /= 10 ) error stop 134
   if ( dta%vdt2%var_c /= 14 .or. dtc%vdt2%var_c /= 14 ) error stop 136
   if ( dta%vdt2%var_d /= 18 .or. dtc%vdt2%var_d /= 18 ) error stop 138
   if ( dta%vdt2%var_e /= 22 .or. dtc%vdt2%var_e /= 22 ) error stop 140
   if ( dta%vdt2%var_f /= 26 .or. dtc%vdt2%var_f /= 26 ) error stop 142
   if ( dta%vdt2%var_g /= 30 .or. dtc%vdt2%var_g /= 30 ) error stop 144
   if ( dta%vdt2%var_h /= 34 .or. dtc%vdt2%var_h /= 34 ) error stop 146

   if ( dta%vdt2%vdt1%var_a /= 7 .or. &
                              dtc%vdt2%vdt1%var_a /= 7 ) error stop 148
   if ( dta%vdt2%vdt1%var_b /= 11 .or. &
                              dtc%vdt2%vdt1%var_b /= 11 ) error stop 150
   if ( dta%vdt2%vdt1%var_c /= 15 .or. &
                              dtc%vdt2%vdt1%var_c /= 15 ) error stop 152
   if ( dta%vdt2%vdt1%var_d /= 19 .or. &
                              dtc%vdt2%vdt1%var_d /= 19 ) error stop 154
   if ( dta%vdt2%vdt1%var_e /= 23 .or. &
                              dtc%vdt2%vdt1%var_e /= 23 ) error stop 156
   if ( dta%vdt2%vdt1%var_f /= 27 .or. &
                              dtc%vdt2%vdt1%var_f /= 27 ) error stop 158
   if ( dta%vdt2%vdt1%var_g /= 31 .or. &
                              dtc%vdt2%vdt1%var_g /= 31 ) error stop 160
   if ( dta%vdt2%vdt1%var_h /= 35 .or. &
                              dtc%vdt2%vdt1%var_h /= 35 ) error stop 162

!! Test 4

   dta = dt0

   dtp = fun4(dta+dta)

   call C_F_POINTER(dtp,dtd)

   if ( dta%var_a /= 5 .or. dtd%var_a /= 5 ) error stop 164
   if ( dta%var_b /= 9 .or. dtd%var_b /= 9 ) error stop 166
   if ( dta%var_c /= 13 .or. dtd%var_c /= 13 ) error stop 168
   if ( dta%var_d /= 17 .or. dtd%var_d /= 17 ) error stop 170
   if ( dta%var_e /= 21 .or. dtd%var_e /= 21 ) error stop 172
   if ( dta%var_f /= 25 .or. dtd%var_f /= 25 ) error stop 174
   if ( dta%var_g /= 29 .or. dtd%var_g /= 29 ) error stop 176
   if ( dta%var_h /= 33 .or. dtd%var_h /= 33 ) error stop 178

   if ( dta%vdt2%var_a /= 6 .or. dtd%vdt2%var_a /= 6 ) error stop 180
   if ( dta%vdt2%var_b /= 10 .or. dtd%vdt2%var_b /= 10 ) error stop 182
   if ( dta%vdt2%var_c /= 14 .or. dtd%vdt2%var_c /= 14 ) error stop 184
   if ( dta%vdt2%var_d /= 18 .or. dtd%vdt2%var_d /= 18 ) error stop 186
   if ( dta%vdt2%var_e /= 22 .or. dtd%vdt2%var_e /= 22 ) error stop 188
   if ( dta%vdt2%var_f /= 26 .or. dtd%vdt2%var_f /= 26 ) error stop 190
   if ( dta%vdt2%var_g /= 30 .or. dtd%vdt2%var_g /= 30 ) error stop 192
   if ( dta%vdt2%var_h /= 34 .or. dtd%vdt2%var_h /= 34 ) error stop 194

   if ( dta%vdt2%vdt1%var_a /= 7 .or. &
                              dtd%vdt2%vdt1%var_a /= 7 ) error stop 196
   if ( dta%vdt2%vdt1%var_b /= 11 .or. &
                              dtd%vdt2%vdt1%var_b /= 11 ) error stop 198
   if ( dta%vdt2%vdt1%var_c /= 15 .or. &
                              dtd%vdt2%vdt1%var_c /= 15 ) error stop 200
   if ( dta%vdt2%vdt1%var_d /= 19 .or. &
                              dtd%vdt2%vdt1%var_d /= 19 ) error stop 202
   if ( dta%vdt2%vdt1%var_e /= 23 .or. &
                              dtd%vdt2%vdt1%var_e /= 23 ) error stop 204
   if ( dta%vdt2%vdt1%var_f /= 27 .or. &
                              dtd%vdt2%vdt1%var_f /= 27 ) error stop 206
   if ( dta%vdt2%vdt1%var_g /= 31 .or. &
                              dtd%vdt2%vdt1%var_g /= 31 ) error stop 208
   if ( dta%vdt2%vdt1%var_h /= 35 .or. &
                              dtd%vdt2%vdt1%var_h /= 35 ) error stop 210

end program fxdtyia41
