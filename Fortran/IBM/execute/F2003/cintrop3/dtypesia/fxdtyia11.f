!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtya00.presh fxdtyia11 cxdtyia11
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
!*      - Testing 2-levels deep derived types with BIND(C) attribute
!*      - Testing 2-levels deep derived types with VALUE, INTENT attributes
!*      - Testing 2-levels deep derived types with integer components
!*      - Testing FORTRAN functions and C functions
!*      - Main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtyia11
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

end module mxdtyia11

function fun1(dt) bind(c)
   use mxdtyia11
   type(dt2) :: fun1, dt

   if ( dt%var_a /= 2 .or. dt%vdt1%var_a /= 2 ) error stop 20
   if ( dt%var_b /= 4 .or. dt%vdt1%var_b /= 4 ) error stop 22
   if ( dt%var_c /= 6 .or. dt%vdt1%var_c /= 6 ) error stop 24
   if ( dt%var_d /= 8 .or. dt%vdt1%var_d /= 8 ) error stop 26
   if ( dt%var_e /= 10 .or. dt%vdt1%var_e /= 10 ) error stop 28
   if ( dt%var_f /= 12 .or. dt%vdt1%var_f /= 12 ) error stop 30
   if ( dt%var_g /= 14 .or. dt%vdt1%var_g /= 14 ) error stop 32
   if ( dt%var_h /= 16 .or. dt%vdt1%var_h /= 16 ) error stop 34

   dt%var_a = dt%var_a + 1
   dt%var_b = dt%var_b + 1
   dt%var_c = dt%var_c + 1
   dt%var_d = dt%var_d + 1
   dt%var_e = dt%var_e + 1
   dt%var_f = dt%var_f + 1
   dt%var_g = dt%var_g + 1
   dt%var_h = dt%var_h + 1

   dt%vdt1%var_a = dt%vdt1%var_a + 2
   dt%vdt1%var_b = dt%vdt1%var_b + 2
   dt%vdt1%var_c = dt%vdt1%var_c + 2
   dt%vdt1%var_d = dt%vdt1%var_d + 2
   dt%vdt1%var_e = dt%vdt1%var_e + 2
   dt%vdt1%var_f = dt%vdt1%var_f + 2
   dt%vdt1%var_g = dt%vdt1%var_g + 2
   dt%vdt1%var_h = dt%vdt1%var_h + 2

   fun1 = dt

end function fun1

function fun2(dt) bind(c)
   use mxdtyia11
   type(dt2), value :: dt
   type(dt2) :: fun2

   if ( dt%var_a /= 2 .or. dt%vdt1%var_a /= 2 ) error stop 36
   if ( dt%var_b /= 4 .or. dt%vdt1%var_b /= 4 ) error stop 38
   if ( dt%var_c /= 6 .or. dt%vdt1%var_c /= 6 ) error stop 40
   if ( dt%var_d /= 8 .or. dt%vdt1%var_d /= 8 ) error stop 42
   if ( dt%var_e /= 10 .or. dt%vdt1%var_e /= 10 ) error stop 44
   if ( dt%var_f /= 12 .or. dt%vdt1%var_f /= 12 ) error stop 46
   if ( dt%var_g /= 14 .or. dt%vdt1%var_g /= 14 ) error stop 48
   if ( dt%var_h /= 16 .or. dt%vdt1%var_h /= 16 ) error stop 50

   dt%var_a = dt%var_a + 1
   dt%var_b = dt%var_b + 1
   dt%var_c = dt%var_c + 1
   dt%var_d = dt%var_d + 1
   dt%var_e = dt%var_e + 1
   dt%var_f = dt%var_f + 1
   dt%var_g = dt%var_g + 1
   dt%var_h = dt%var_h + 1

   dt%vdt1%var_a = dt%vdt1%var_a + 2
   dt%vdt1%var_b = dt%vdt1%var_b + 2
   dt%vdt1%var_c = dt%vdt1%var_c + 2
   dt%vdt1%var_d = dt%vdt1%var_d + 2
   dt%vdt1%var_e = dt%vdt1%var_e + 2
   dt%vdt1%var_f = dt%vdt1%var_f + 2
   dt%vdt1%var_g = dt%vdt1%var_g + 2
   dt%vdt1%var_h = dt%vdt1%var_h + 2

   fun2 = dt

end function fun2

function fun3(dtx,dty) bind(c)
   use mxdtyia11
   type(dt2), intent(in) :: dtx
   type(dt2), intent(out) :: dty
   type(dt2) :: fun3

   if ( dtx%var_a /= 4 .or. dtx%vdt1%var_a /= 4 ) error stop 52
   if ( dtx%var_b /= 8 .or. dtx%vdt1%var_b /= 8 ) error stop 54
   if ( dtx%var_c /= 12 .or. dtx%vdt1%var_c /= 12 ) error stop 56
   if ( dtx%var_d /= 16 .or. dtx%vdt1%var_d /= 16 ) error stop 58
   if ( dtx%var_e /= 20 .or. dtx%vdt1%var_e /= 20 ) error stop 60
   if ( dtx%var_f /= 24 .or. dtx%vdt1%var_f /= 24 ) error stop 62
   if ( dtx%var_g /= 28 .or. dtx%vdt1%var_g /= 28 ) error stop 64
   if ( dtx%var_h /= 32 .or. dtx%vdt1%var_h /= 32 ) error stop 66

   dty%var_a = dtx%var_a + 1
   dty%var_b = dtx%var_b + 1
   dty%var_c = dtx%var_c + 1
   dty%var_d = dtx%var_d + 1
   dty%var_e = dtx%var_e + 1
   dty%var_f = dtx%var_f + 1
   dty%var_g = dtx%var_g + 1
   dty%var_h = dtx%var_h + 1

   dty%vdt1%var_a = dtx%vdt1%var_a + 2
   dty%vdt1%var_b = dtx%vdt1%var_b + 2
   dty%vdt1%var_c = dtx%vdt1%var_c + 2
   dty%vdt1%var_d = dtx%vdt1%var_d + 2
   dty%vdt1%var_e = dtx%vdt1%var_e + 2
   dty%vdt1%var_f = dtx%vdt1%var_f + 2
   dty%vdt1%var_g = dtx%vdt1%var_g + 2
   dty%vdt1%var_h = dtx%vdt1%var_h + 2

   fun3 = dty

end function fun3

function fun4(dtx,dty) bind(c)
   use mxdtyia11
   type(dt2), intent(in) :: dtx
   type(dt2), intent(out) :: dty
   type(dt2), target, static :: dtz
   type(C_PTR) :: fun4

   if ( dtx%var_a /= 4 .or. dtx%vdt1%var_a /= 4 ) error stop 68
   if ( dtx%var_b /= 8 .or. dtx%vdt1%var_b /= 8 ) error stop 70
   if ( dtx%var_c /= 12 .or. dtx%vdt1%var_c /= 12 ) error stop 72
   if ( dtx%var_d /= 16 .or. dtx%vdt1%var_d /= 16 ) error stop 74
   if ( dtx%var_e /= 20 .or. dtx%vdt1%var_e /= 20 ) error stop 76
   if ( dtx%var_f /= 24 .or. dtx%vdt1%var_f /= 24 ) error stop 78
   if ( dtx%var_g /= 28 .or. dtx%vdt1%var_g /= 28 ) error stop 80
   if ( dtx%var_h /= 32 .or. dtx%vdt1%var_h /= 32 ) error stop 82

   dtz%var_a = dtx%var_a + 1
   dtz%var_b = dtx%var_b + 1
   dtz%var_c = dtx%var_c + 1
   dtz%var_d = dtx%var_d + 1
   dtz%var_e = dtx%var_e + 1
   dtz%var_f = dtx%var_f + 1
   dtz%var_g = dtx%var_g + 1
   dtz%var_h = dtx%var_h + 1

   dtz%vdt1%var_a = dtx%vdt1%var_a + 2
   dtz%vdt1%var_b = dtx%vdt1%var_b + 2
   dtz%vdt1%var_c = dtx%vdt1%var_c + 2
   dtz%vdt1%var_d = dtx%vdt1%var_d + 2
   dtz%vdt1%var_e = dtx%vdt1%var_e + 2
   dtz%vdt1%var_f = dtx%vdt1%var_f + 2
   dtz%vdt1%var_g = dtx%vdt1%var_g + 2
   dtz%vdt1%var_h = dtx%vdt1%var_h + 2

   dty = dtz

   fun4 = C_LOC(dtz)

end function fun4
