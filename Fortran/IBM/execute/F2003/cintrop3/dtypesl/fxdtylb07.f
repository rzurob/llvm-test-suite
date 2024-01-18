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
!*      - Testing 3-levels deep derived types with BIND(C) attribute
!*      - Testing 3-levels deep derived types with VALUE, INTENT attributes
!*      - Testing 3-levels deep derived types with logical, character and real components
!*      - Testing FORTRAN subroutines and C functions
!*      - Main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtylb07
   use ISO_C_BINDING

   type, bind(c) :: dt1
      logical(C_BOOL) var_a
      real(C_DOUBLE) var_b
      character(C_CHAR) var_c
      real(C_FLOAT) var_d
      real(16) var_e
      real(8) var_f
      logical(C_BOOL) var_g
      real(4) var_h
   end type

   type, bind(c) :: dt2
      real(C_DOUBLE) var_a
      logical(C_BOOL) var_b
      real(8) var_c
      character(C_CHAR) var_d
      real(16) var_e
      real(4) var_f
      logical(C_BOOL) var_g
      real(C_FLOAT) var_h
      type(dt1) :: vdt1
   end type

   type, bind(c) :: dt3
      type(dt2) :: vdt2
      real(C_FLOAT) var_a
      logical(C_BOOL) var_b
      real(16) var_c
      real(8) var_d
      real(4) var_e
      logical(C_BOOL) var_f
      real(C_DOUBLE) var_g
      character(C_CHAR) var_h
   end type

end module mxdtylb07

!! Test 1 Fun
function fun1(dt) bind(c)
   use mxdtylb07
   type(dt3) :: fun1, dt

   if ( dt%var_a /= 2.0e0 .or. dt%vdt2%var_a /= 2.0d0 .or. &
                          (dt%vdt2%vdt1%var_a .neqv. .false.) ) error stop 20
   if ( (dt%var_b .neqv. .false.) .or. (dt%vdt2%var_b .neqv. .false.) .or. &
                          dt%vdt2%vdt1%var_b /= 4.0d0 ) error stop 22
   if ( dt%var_c /= 6.0q0 .or. dt%vdt2%var_c /= 6.0d0 .or. &
                          (dt%vdt2%vdt1%var_c /= 'A') ) error stop 24
   if ( dt%var_d /= 8.0d0 .or. (dt%vdt2%var_d /= 'A') .or. &
                          dt%vdt2%vdt1%var_d /= 8.0e0 ) error stop 26
   if ( dt%var_e /= 10.0e0 .or. dt%vdt2%var_e /= 10.0q0 .or. &
                           dt%vdt2%vdt1%var_e /= 10.0q0 ) error stop 28
   if ( (dt%var_f .neqv. .false.) .or. dt%vdt2%var_f /= 12.0e0 .or. &
                           dt%vdt2%vdt1%var_f /= 12.0d0 ) error stop 30
   if ( dt%var_g /= 14.0d0 .or. (dt%vdt2%var_g .neqv. .false.) .or. &
                           dt%vdt2%vdt1%var_h /= 16.0e0 ) error stop 32
   if ( (dt%var_h /= 'A') .or. dt%vdt2%var_h /= 16.0e0 .or. &
                           dt%vdt2%vdt1%var_h /= 16.0e0 ) error stop 34

   dt%var_a = dt%var_a + 1.0e0
   dt%var_b = .not. dt%var_b
   dt%var_c = dt%var_c + 1.0q0
   dt%var_d = dt%var_d + 1.0d0
   dt%var_e = dt%var_e + 1.0e0
   dt%var_f = .not. dt%var_f
   dt%var_g = dt%var_g + 1.0d0
   dt%var_h = 'B'

   dt%vdt2%var_a = dt%vdt2%var_a + 2.0d0
   dt%vdt2%var_b = .not. dt%vdt2%var_b
   dt%vdt2%var_c = dt%vdt2%var_c + 2.0d0
   dt%vdt2%var_d = 'B'
   dt%vdt2%var_e = dt%vdt2%var_e + 2.0q0
   dt%vdt2%var_f = dt%vdt2%var_f + 2.0e0
   dt%vdt2%var_g = .not. dt%vdt2%var_g
   dt%vdt2%var_h = dt%vdt2%var_h + 2.0e0

   dt%vdt2%vdt1%var_a = .not. dt%vdt2%vdt1%var_a
   dt%vdt2%vdt1%var_b = dt%vdt2%vdt1%var_b + 3.0d0
   dt%vdt2%vdt1%var_c = 'B'
   dt%vdt2%vdt1%var_d = dt%vdt2%vdt1%var_d + 3.0e0
   dt%vdt2%vdt1%var_e = dt%vdt2%vdt1%var_e + 3.0q0
   dt%vdt2%vdt1%var_f = dt%vdt2%vdt1%var_f + 3.0d0
   dt%vdt2%vdt1%var_g = .not. dt%vdt2%vdt1%var_g
   dt%vdt2%vdt1%var_h = dt%vdt2%vdt1%var_h + 3.0e0

   fun1 = dt

end function fun1
!----------------------------------------------------------

!! Test 2 Fun
function fun2(dt) bind(c)
   use mxdtylb07
   type(dt3), value :: dt
   type(dt3) :: fun2

   if ( dt%var_a /= 2.0e0 .or. dt%vdt2%var_a /= 2.0d0 .or. &
                          (dt%vdt2%vdt1%var_a .neqv. .false.) ) error stop 36
   if ( (dt%var_b .neqv. .false.) .or. (dt%vdt2%var_b .neqv. .false.) .or. &
                          dt%vdt2%vdt1%var_b /= 4.0d0 ) error stop 38
   if ( dt%var_c /= 6.0q0 .or. dt%vdt2%var_c /= 6.0d0 .or. &
                          (dt%vdt2%vdt1%var_c /= 'A') ) error stop 40
   if ( dt%var_d /= 8.0d0 .or. (dt%vdt2%var_d /= 'A') .or. &
                          dt%vdt2%vdt1%var_d /= 8.0e0 ) error stop 42
   if ( dt%var_e /= 10.0e0 .or. dt%vdt2%var_e /= 10.0q0 .or. &
                           dt%vdt2%vdt1%var_e /= 10.0q0 ) error stop 44
   if ( (dt%var_f .neqv. .false.) .or. dt%vdt2%var_f /= 12.0e0 .or. &
                           dt%vdt2%vdt1%var_f /= 12.0d0 ) error stop 46
   if ( dt%var_g /= 14.0d0 .or. (dt%vdt2%var_g .neqv. .false.) .or. &
                           dt%vdt2%vdt1%var_h /= 16.0e0 ) error stop 48
   if ( (dt%var_h /= 'A') .or. dt%vdt2%var_h /= 16.0e0 .or. &
                           dt%vdt2%vdt1%var_h /= 16.0e0 ) error stop 50

   dt%var_a = dt%var_a + 1.0e0
   dt%var_b = .not. dt%var_b
   dt%var_c = dt%var_c + 1.0q0
   dt%var_d = dt%var_d + 1.0d0
   dt%var_e = dt%var_e + 1.0e0
   dt%var_f = .not. dt%var_f
   dt%var_g = dt%var_g + 1.0d0
   dt%var_h = 'B'

   dt%vdt2%var_a = dt%vdt2%var_a + 2.0d0
   dt%vdt2%var_b = .not. dt%vdt2%var_b
   dt%vdt2%var_c = dt%vdt2%var_c + 2.0d0
   dt%vdt2%var_d = 'B'
   dt%vdt2%var_e = dt%vdt2%var_e + 2.0q0
   dt%vdt2%var_f = dt%vdt2%var_f + 2.0e0
   dt%vdt2%var_g = .not. dt%vdt2%var_g
   dt%vdt2%var_h = dt%vdt2%var_h + 2.0e0

   dt%vdt2%vdt1%var_a = .not. dt%vdt2%vdt1%var_a
   dt%vdt2%vdt1%var_b = dt%vdt2%vdt1%var_b + 3.0d0
   dt%vdt2%vdt1%var_c = 'B'
   dt%vdt2%vdt1%var_d = dt%vdt2%vdt1%var_d + 3.0e0
   dt%vdt2%vdt1%var_e = dt%vdt2%vdt1%var_e + 3.0q0
   dt%vdt2%vdt1%var_f = dt%vdt2%vdt1%var_f + 3.0d0
   dt%vdt2%vdt1%var_g = .not. dt%vdt2%vdt1%var_g
   dt%vdt2%vdt1%var_h = dt%vdt2%vdt1%var_h + 3.0e0

   fun2 = dt

end function fun2
!----------------------------------------------------------

!! Test 3 Fun
function fun3(dtx,dty) bind(c)
   use mxdtylb07
   type(dt3), intent(in) :: dtx
   type(dt3), intent(out) :: dty
   type(dt3) :: fun3

   if ( dtx%var_a /= 4.0e0 .or. dtx%vdt2%var_a /= 4.0d0 .or. &
                            (dtx%vdt2%vdt1%var_a .neqv. .false.) ) error stop 52
   if ( (dtx%var_b .neqv. .false.) .or. (dtx%vdt2%var_b .neqv. .false.) .or. &
                            dtx%vdt2%vdt1%var_b /= 8.0d0 ) error stop 54
   if ( dtx%var_c /= 12.0q0 .or. dtx%vdt2%var_c /= 12.0d0 .or. &
                             (dtx%vdt2%vdt1%var_c /= 'A') ) error stop 56
   if ( dtx%var_d /= 16.0d0 .or. (dtx%vdt2%var_d /= 'A') .or. &
                             dtx%vdt2%vdt1%var_d /= 16.0e0 ) error stop 58
   if ( dtx%var_e /= 20.0e0 .or. dtx%vdt2%var_e /= 20.0q0 .or. &
                             dtx%vdt2%vdt1%var_e /= 20.0q0 ) error stop 60
   if ( (dtx%var_f .neqv. .false.) .or. dtx%vdt2%var_f /= 24.0e0 .or. &
                             dtx%vdt2%vdt1%var_f /= 24.0d0 ) error stop 62
   if ( dtx%var_g /= 28.0d0 .or. (dtx%vdt2%var_g .neqv. .false.) .or. &
                             (dtx%vdt2%vdt1%var_g .neqv. .false.) ) error stop 64
   if ( (dtx%var_h /= 'A') .or. dtx%vdt2%var_h /= 32.0e0 .or. &
                             dtx%vdt2%vdt1%var_h /= 32.0e0 ) error stop 66

   dty%var_a = dtx%var_a + 1.0e0
   dty%var_b = .not. dtx%var_b
   dty%var_c = dtx%var_c + 1.0q0
   dty%var_d = dtx%var_d + 1.0d0
   dty%var_e = dtx%var_e + 1.0e0
   dty%var_f = .not. dtx%var_f
   dty%var_g = dtx%var_g + 1.0d0
   dty%var_h = 'B'

   dty%vdt2%var_a = dtx%vdt2%var_a + 2.0d0
   dty%vdt2%var_b = .not. dtx%vdt2%var_b
   dty%vdt2%var_c = dtx%vdt2%var_c + 2.0d0
   dty%vdt2%var_d = 'B'
   dty%vdt2%var_e = dtx%vdt2%var_e + 2.0q0
   dty%vdt2%var_f = dtx%vdt2%var_f + 2.0e0
   dty%vdt2%var_g = .not. dtx%vdt2%var_g
   dty%vdt2%var_h = dtx%vdt2%var_h + 2.0e0

   dty%vdt2%vdt1%var_a = .not. dtx%vdt2%vdt1%var_a
   dty%vdt2%vdt1%var_b = dtx%vdt2%vdt1%var_b + 3.0d0
   dty%vdt2%vdt1%var_c = 'B'
   dty%vdt2%vdt1%var_d = dtx%vdt2%vdt1%var_d + 3.0e0
   dty%vdt2%vdt1%var_e = dtx%vdt2%vdt1%var_e + 3.0q0
   dty%vdt2%vdt1%var_f = dtx%vdt2%vdt1%var_f + 3.0d0
   dty%vdt2%vdt1%var_g = .not. dtx%vdt2%vdt1%var_g
   dty%vdt2%vdt1%var_h = dtx%vdt2%vdt1%var_h + 3.0e0

   fun3 = dty

end function fun3
!----------------------------------------------------------

!! Test 4 Fun
function fun4(dtx,dty) bind(c)
   use mxdtylb07
   type(dt3), intent(in) :: dtx
   type(dt3), intent(out) :: dty
   type(dt3), target, static :: dtz
   type(C_PTR) :: fun4

   if ( dtx%var_a /= 4.0e0 .or. dtx%vdt2%var_a /= 4.0d0 .or. &
                            (dtx%vdt2%vdt1%var_a .neqv. .false.) ) error stop 68
   if ( (dtx%var_b .neqv. .false.) .or. (dtx%vdt2%var_b .neqv. .false.) .or. &
                            dtx%vdt2%vdt1%var_b /= 8.0d0 ) error stop 70
   if ( dtx%var_c /= 12.0q0 .or. dtx%vdt2%var_c /= 12.0d0 .or. &
                             (dtx%vdt2%vdt1%var_c /= 'A') ) error stop 72
   if ( dtx%var_d /= 16.0d0 .or. (dtx%vdt2%var_d /= 'A') .or. &
                             dtx%vdt2%vdt1%var_d /= 16.0e0 ) error stop 74
   if ( dtx%var_e /= 20.0e0 .or. dtx%vdt2%var_e /= 20.0q0 .or. &
                             dtx%vdt2%vdt1%var_e /= 20.0q0 ) error stop 76
   if ( (dtx%var_f .neqv. .false.) .or. dtx%vdt2%var_f /= 24.0e0 .or. &
                             dtx%vdt2%vdt1%var_f /= 24.0d0 ) error stop 78
   if ( dtx%var_g /= 28.0d0 .or. (dtx%vdt2%var_g .neqv. .false.) .or. &
                             (dtx%vdt2%vdt1%var_g .neqv. .false.) ) error stop 80
   if ( (dtx%var_h /= 'A') .or. dtx%vdt2%var_h /= 32.0e0 .or. &
                             dtx%vdt2%vdt1%var_h /= 32.0e0 ) error stop 82

   dtz%var_a = dtx%var_a + 1.0e0
   dtz%var_b = .not. dtx%var_b
   dtz%var_c = dtx%var_c + 1.0q0
   dtz%var_d = dtx%var_d + 1.0d0
   dtz%var_e = dtx%var_e + 1.0e0
   dtz%var_f = .not. dtx%var_f
   dtz%var_g = dtx%var_g + 1.0d0
   dtz%var_h = 'B'

   dtz%vdt2%var_a = dtx%vdt2%var_a + 2.0d0
   dtz%vdt2%var_b = .not. dtx%vdt2%var_b
   dtz%vdt2%var_c = dtx%vdt2%var_c + 2.0d0
   dtz%vdt2%var_d = 'B'
   dtz%vdt2%var_e = dtx%vdt2%var_e + 2.0q0
   dtz%vdt2%var_f = dtx%vdt2%var_f + 2.0e0
   dtz%vdt2%var_g = .not. dtx%vdt2%var_g
   dtz%vdt2%var_h = dtx%vdt2%var_h + 2.0e0

   dtz%vdt2%vdt1%var_a = .not. dtx%vdt2%vdt1%var_a
   dtz%vdt2%vdt1%var_b = dtx%vdt2%vdt1%var_b + 3.0d0
   dtz%vdt2%vdt1%var_c = 'B'
   dtz%vdt2%vdt1%var_d = dtx%vdt2%vdt1%var_d + 3.0e0
   dtz%vdt2%vdt1%var_e = dtx%vdt2%vdt1%var_e + 3.0q0
   dtz%vdt2%vdt1%var_f = dtx%vdt2%vdt1%var_f + 3.0d0
   dtz%vdt2%vdt1%var_g = .not. dtx%vdt2%vdt1%var_g
   dtz%vdt2%vdt1%var_h = dtx%vdt2%vdt1%var_h + 3.0e0

   dty = dtz

   fun4 = C_LOC(dtz)

end function fun4
!----------------------------------------------------------
