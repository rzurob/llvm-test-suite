!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtye00.presh fxdtyla11 cxdtyla11
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
!*      - Testing 2-levels deep derived types with logical, character and real components
!*      - Testing FORTRAN functions and C functions
!*      - Main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtyla11
   use ISO_C_BINDING

   type, bind(c) :: dt1
      logical(C_BOOL) var_a
      real(C_DOUBLE) var_b
      character(C_CHAR) var_c
      real(C_FLOAT) var_d
      real(C_LONG_DOUBLE) var_e
      real(8) var_f
      logical(C_BOOL) var_g
      real(4) var_h
   end type

   type, bind(c) :: dt2
      real(C_FLOAT) var_a
      logical(C_BOOL) var_b
      real(C_LONG_DOUBLE) var_c
      real(8) var_d
      real(4) var_e
      logical(C_BOOL) var_f
      real(C_DOUBLE) var_g
      character(C_CHAR) var_h
      type(dt1) :: vdt1
   end type

end module mxdtyla11

!! Test 1 Fun
function fun1(dt) bind(c)
   use mxdtyla11
   type(dt2) :: fun1, dt

   if ( dt%var_a /= 2.0e0 .or. (dt%vdt1%var_a .neqv. .false.) ) error stop 20
   if ( (dt%var_b .neqv. .false.) .or. dt%vdt1%var_b /= 4 ) error stop 22
   if ( dt%var_c /= 6.0q0 .or. (dt%vdt1%var_c /= 'A') ) error stop 24
   if ( dt%var_d /= 8.0d0 .or. dt%vdt1%var_d /= 8 ) error stop 26
   if ( dt%var_e /= 10.0e0 .or. dt%vdt1%var_e /= 10 ) error stop 28
   if ( (dt%var_f .neqv. .false.) .or. dt%vdt1%var_f /= 12 ) error stop 30
   if ( dt%var_g /= 14.0d0 .or. (dt%vdt1%var_g .neqv. .false.) ) error stop 32
   if ( (dt%var_h /= 'A') .or. dt%vdt1%var_h /= 16 ) error stop 34

   dt%var_a = dt%var_a + 1.0e0
   dt%var_b = .not. dt%var_b
   dt%var_c = dt%var_c + 1.0q0
   dt%var_d = dt%var_d + 1.0d0
   dt%var_e = dt%var_e + 1.0e0
   dt%var_f = .not. dt%var_f
   dt%var_g = dt%var_g + 1.0d0
   dt%var_h = 'B'

   dt%vdt1%var_a = .not. dt%vdt1%var_a
   dt%vdt1%var_b = dt%vdt1%var_b + 2
   dt%vdt1%var_c = 'B'
   dt%vdt1%var_d = dt%vdt1%var_d + 2
   dt%vdt1%var_e = dt%vdt1%var_e + 2
   dt%vdt1%var_f = dt%vdt1%var_f + 2
   dt%vdt1%var_g = .not. dt%vdt1%var_g
   dt%vdt1%var_h = dt%vdt1%var_h + 2

   fun1 = dt

end function fun1

!! Test 2 Fun
function fun2(dt) bind(c)
   use mxdtyla11
   type(dt2), value :: dt
   type(dt2) :: fun2

   if ( dt%var_a /= 2.0e0 .or. (dt%vdt1%var_a .neqv. .false.) ) error stop 36
   if ( (dt%var_b .neqv. .false.) .or. dt%vdt1%var_b /= 4 ) error stop 38
   if ( dt%var_c /= 6.0q0 .or. (dt%vdt1%var_c /= 'A') ) error stop 40
   if ( dt%var_d /= 8.0d0 .or. dt%vdt1%var_d /= 8 ) error stop 42
   if ( dt%var_e /= 10.0e0 .or. dt%vdt1%var_e /= 10 ) error stop 44
   if ( (dt%var_f .neqv. .false.) .or. dt%vdt1%var_f /= 12 ) error stop 46
   if ( dt%var_g /= 14.0d0 .or. (dt%vdt1%var_g .neqv. .false.) ) error stop 48
   if ( (dt%var_h /= 'A') .or. dt%vdt1%var_h /= 16 ) error stop 50

   dt%var_a = dt%var_a + 1.0e0
   dt%var_b = .not. dt%var_b
   dt%var_c = dt%var_c + 1.0q0
   dt%var_d = dt%var_d + 1.0d0
   dt%var_e = dt%var_e + 1.0e0
   dt%var_f = .not. dt%var_f
   dt%var_g = dt%var_g + 1.0d0
   dt%var_h = 'B'

   dt%vdt1%var_a = .not. dt%vdt1%var_a
   dt%vdt1%var_b = dt%vdt1%var_b + 2
   dt%vdt1%var_c = 'B'
   dt%vdt1%var_d = dt%vdt1%var_d + 2
   dt%vdt1%var_e = dt%vdt1%var_e + 2
   dt%vdt1%var_f = dt%vdt1%var_f + 2
   dt%vdt1%var_g = .not. dt%vdt1%var_g
   dt%vdt1%var_h = dt%vdt1%var_h + 2

   fun2 = dt

end function fun2

!! Test 3 Fun
function fun3(dtx,dty) bind(c)
   use mxdtyla11
   type(dt2), intent(in) :: dtx
   type(dt2), intent(out) :: dty
   type(dt2) :: fun3

   if ( dtx%var_a /= 4.0e0 .or. (dtx%vdt1%var_a .neqv. .false.) ) error stop 52
   if ( (dtx%var_b .neqv. .false.) .or. dtx%vdt1%var_b /= 8 ) error stop 54
   if ( dtx%var_c /= 12.0q0 .or. (dtx%vdt1%var_c /= 'A') ) error stop 56
   if ( dtx%var_d /= 16.0d0 .or. dtx%vdt1%var_d /= 16 ) error stop 58
   if ( dtx%var_e /= 20.0e0 .or. dtx%vdt1%var_e /= 20 ) error stop 60
   if ( (dtx%var_f .neqv. .false.) .or. dtx%vdt1%var_f /= 24 ) error stop 62
   if ( dtx%var_g /= 28.0d0 .or. (dtx%vdt1%var_g .neqv. .false.) ) error stop 64
   if ( (dtx%var_h /= 'A') .or. dtx%vdt1%var_h /= 32 ) error stop 66

   dty%var_a = dtx%var_a + 1.0e0
   dty%var_b = .not. dtx%var_b
   dty%var_c = dtx%var_c + 1.0q0
   dty%var_d = dtx%var_d + 1.0d0
   dty%var_e = dtx%var_e + 1.0e0
   dty%var_f = .not. dtx%var_f
   dty%var_g = dtx%var_g + 1.0d0
   dty%var_h = 'B'

   dty%vdt1%var_a = .not. dtx%vdt1%var_a
   dty%vdt1%var_b = dtx%vdt1%var_b + 2
   dty%vdt1%var_c = 'B'
   dty%vdt1%var_d = dtx%vdt1%var_d + 2
   dty%vdt1%var_e = dtx%vdt1%var_e + 2
   dty%vdt1%var_f = dtx%vdt1%var_f + 2
   dty%vdt1%var_g = .not. dtx%vdt1%var_g
   dty%vdt1%var_h = dtx%vdt1%var_h + 2

   fun3 = dty

end function fun3

!! Test 4 Fun
function fun4(dtx,dty) bind(c)
   use mxdtyla11
   type(dt2), intent(in) :: dtx
   type(dt2), intent(out) :: dty
   type(dt2), target, static :: dtz
   type(C_PTR) :: fun4

   if ( dtx%var_a /= 4.0e0 .or. (dtx%vdt1%var_a .neqv. .false.) ) error stop 68
   if ( (dtx%var_b .neqv. .false.) .or. dtx%vdt1%var_b /= 8 ) error stop 70
   if ( dtx%var_c /= 12.0q0 .or. (dtx%vdt1%var_c /= 'A') ) error stop 72
   if ( dtx%var_d /= 16.0d0 .or. dtx%vdt1%var_d /= 16 ) error stop 74
   if ( dtx%var_e /= 20.0e0 .or. dtx%vdt1%var_e /= 20 ) error stop 76
   if ( (dtx%var_f .neqv. .false.) .or. dtx%vdt1%var_f /= 24 ) error stop 78
   if ( dtx%var_g /= 28.0d0 .or. (dtx%vdt1%var_g .neqv. .false.) ) error stop 80
   if ( (dtx%var_h /= 'A') .or. dtx%vdt1%var_h /= 32 ) error stop 82

   dtz%var_a = dtx%var_a + 1.0e0
   dtz%var_b = .not. dtx%var_b
   dtz%var_c = dtx%var_c + 1.0q0
   dtz%var_d = dtx%var_d + 1.0d0
   dtz%var_e = dtx%var_e + 1.0e0
   dtz%var_f = .not. dtx%var_f
   dtz%var_g = dtx%var_g + 1.0d0
   dtz%var_h = 'B'

   dtz%vdt1%var_a = .not. dtx%vdt1%var_a
   dtz%vdt1%var_b = dtx%vdt1%var_b + 2
   dtz%vdt1%var_c = 'B'
   dtz%vdt1%var_d = dtx%vdt1%var_d + 2
   dtz%vdt1%var_e = dtx%vdt1%var_e + 2
   dtz%vdt1%var_f = dtx%vdt1%var_f + 2
   dtz%vdt1%var_g = .not. dtx%vdt1%var_g
   dtz%vdt1%var_h = dtx%vdt1%var_h + 2

   dty = dtz

   fun4 = C_LOC(dtz)

end function fun4
