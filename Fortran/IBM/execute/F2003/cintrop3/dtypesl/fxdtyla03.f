!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtye00.presh fxdtyla03 cxdtyla03
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
!*      - Testing single derived types with BIND(C) attribute
!*      - Testing single derived types with VALUE, INTENT attributes
!*      - Testing single derived types with logical, character and real components
!*      - Testing FORTRAN functions and C functions
!*      - Main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxdtyla03
   use ISO_C_BINDING

   type, bind(c) :: dt1
      real(C_FLOAT) var_a
      logical(C_BOOL) var_b
      real(C_LONG_DOUBLE) var_c
      real(8) var_d
      real(4) var_e
      logical(C_BOOL) var_f
      real(C_DOUBLE) var_g
      character(C_CHAR) var_h
   end type

end module mxdtyla03

!! Test 1 Fun
function fun1(dt) bind(c)
   use mxdtyla03
   type(dt1), target :: dt
   type(C_PTR) :: fun1

   if ( dt%var_a /= 2.0e0 ) error stop 20
   if ( (dt%var_b .neqv. .false.) ) error stop 22
   if ( dt%var_c /= 6.0q0 ) error stop 24
   if ( dt%var_d /= 8.0d0 ) error stop 26
   if ( dt%var_e /= 10.0e0 ) error stop 28
   if ( (dt%var_f .neqv. .false.) ) error stop 30
   if ( dt%var_g /= 14.0d0 ) error stop 32
   if ( (dt%var_h /= 'A') ) error stop 34

   dt%var_a = dt%var_a + 1.0e0
   dt%var_b = .not. dt%var_b
   dt%var_c = dt%var_c + 1.0q0
   dt%var_d = dt%var_d + 1.0d0
   dt%var_e = dt%var_e + 1.0e0
   dt%var_f = .not. dt%var_f
   dt%var_g = dt%var_g + 1.0d0
   dt%var_h = 'B'

   fun1 = C_LOC(dt)
end function fun1

!! Test 2 Fun
function fun2(dt) bind(c)
   use mxdtyla03
   type(dt1), value, target :: dt
   type(dt1), target, static :: dtw
   type(C_PTR) :: fun2

   if ( dt%var_a /= 2.0e0 ) error stop 36
   if ( (dt%var_b .neqv. .false.) ) error stop 38
   if ( dt%var_c /= 6.0q0 ) error stop 40
   if ( dt%var_d /= 8.0d0 ) error stop 42
   if ( dt%var_e /= 10.0e0 ) error stop 44
   if ( (dt%var_f .neqv. .false.) ) error stop 46
   if ( dt%var_g /= 14.0d0 ) error stop 48
   if ( (dt%var_h /= 'A') ) error stop 50

   dt%var_a = dt%var_a + 1.0e0
   dt%var_b = .not. dt%var_b
   dt%var_c = dt%var_c + 1.0q0
   dt%var_d = dt%var_d + 1.0d0
   dt%var_e = dt%var_e + 1.0e0
   dt%var_f = .not. dt%var_f
   dt%var_g = dt%var_g + 1.0d0
   dt%var_h = 'B'

   dtw = dt
   fun2 = C_LOC(dtw)
end function fun2

!! Test 3 Fun
function fun3(dtx,dty) bind(c)
   use mxdtyla03
   type(dt1), intent(in) :: dtx
   type(dt1), intent(out), target :: dty
   type(C_PTR) :: fun3

   if ( dtx%var_a /= 4.0e0 ) error stop 52
   if ( (dtx%var_b .neqv. .false.) ) error stop 54
   if ( dtx%var_c /= 12.0q0 ) error stop 56
   if ( dtx%var_d /= 16.0d0 ) error stop 58
   if ( dtx%var_e /= 20.0e0 ) error stop 60
   if ( (dtx%var_f .neqv. .false.) ) error stop 62
   if ( dtx%var_g /= 28.0d0 ) error stop 64
   if ( (dtx%var_h /= 'A') ) error stop 66

   dty%var_a = dtx%var_a + 1.0e0
   dty%var_b = .not. dtx%var_b
   dty%var_c = dtx%var_c + 1.0q0
   dty%var_d = dtx%var_d + 1.0d0
   dty%var_e = dtx%var_e + 1.0e0
   dty%var_f = .not. dtx%var_f
   dty%var_g = dtx%var_g + 1.0d0
   dty%var_h = 'B'

   fun3 = C_LOC(dty)
end function fun3

!! Test 4 Fun
function fun4(dtx,dty) bind(c)
   use mxdtyla03
   type(dt1), intent(in) :: dtx
   type(dt1), intent(out) :: dty
   type(dt1), target, static :: dtz
   type(C_PTR) :: fun4

   if ( dtx%var_a /= 4.0e0 ) error stop 68
   if ( (dtx%var_b .neqv. .false.) ) error stop 70
   if ( dtx%var_c /= 12.0q0 ) error stop 72
   if ( dtx%var_d /= 16.0d0 ) error stop 74
   if ( dtx%var_e /= 20.0e0 ) error stop 76
   if ( (dtx%var_f .neqv. .false.) ) error stop 78
   if ( dtx%var_g /= 28.0d0 ) error stop 80
   if ( (dtx%var_h /= 'A') ) error stop 82

   dty%var_a = dtx%var_a + 1.0e0
   dty%var_b = .not. dtx%var_b
   dty%var_c = dtx%var_c + 1.0q0
   dty%var_d = dtx%var_d + 1.0d0
   dty%var_e = dtx%var_e + 1.0e0
   dty%var_f = .not. dtx%var_f
   dty%var_g = dtx%var_g + 1.0d0
   dty%var_h = 'B'

   dtz%var_a = dty%var_a + 1.0e0
   dtz%var_b = .not. dty%var_b
   dtz%var_c = dty%var_c + 1.0q0
   dtz%var_d = dty%var_d + 1.0d0
   dtz%var_e = dty%var_e + 1.0e0
   dtz%var_f = .not. dty%var_f
   dtz%var_g = dty%var_g + 1.0d0
   dtz%var_h = 'B'

   fun4 = C_LOC(dtz)
end function fun4
