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
!*      - Testing single derived types with BIND(C) attribute
!*      - Testing single derived types with VALUE, INTENT attributes
!*      - Testing single derived types with integer components
!*      - Testing FORTRAN subroutines and C void functions
!*      - Main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxdtyia02
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

end module mxdtyia02

subroutine sub1(dt) bind(c)
   use mxdtyia02
   type(dt1) :: dt

   if ( dt%var_a /= 2 ) error stop 20
   if ( dt%var_b /= 4 ) error stop 22
   if ( dt%var_c /= 6 ) error stop 24
   if ( dt%var_d /= 8 ) error stop 26
   if ( dt%var_e /= 10 ) error stop 28
   if ( dt%var_f /= 12 ) error stop 30
   if ( dt%var_g /= 14 ) error stop 32
   if ( dt%var_h /= 16 ) error stop 34

   dt%var_a = dt%var_a + 1
   dt%var_b = dt%var_b + 1
   dt%var_c = dt%var_c + 1
   dt%var_d = dt%var_d + 1
   dt%var_e = dt%var_e + 1
   dt%var_f = dt%var_f + 1
   dt%var_g = dt%var_g + 1
   dt%var_h = dt%var_h + 1

end subroutine sub1

subroutine sub2(dt) bind(c)
   use mxdtyia02
   type(dt1), value :: dt

   if ( dt%var_a /= 2 ) error stop 36
   if ( dt%var_b /= 4 ) error stop 38
   if ( dt%var_c /= 6 ) error stop 40
   if ( dt%var_d /= 8 ) error stop 42
   if ( dt%var_e /= 10 ) error stop 44
   if ( dt%var_f /= 12 ) error stop 46
   if ( dt%var_g /= 14 ) error stop 48
   if ( dt%var_h /= 16 ) error stop 50

   dt%var_a = dt%var_a + 1
   dt%var_b = dt%var_b + 1
   dt%var_c = dt%var_c + 1
   dt%var_d = dt%var_d + 1
   dt%var_e = dt%var_e + 1
   dt%var_f = dt%var_f + 1
   dt%var_g = dt%var_g + 1
   dt%var_h = dt%var_h + 1

end subroutine sub2

subroutine sub3(dtx,dty) bind(c)
   use mxdtyia02
   type(dt1), intent(in) :: dtx
   type(dt1), intent(out) :: dty

   if ( dtx%var_a /= 4 ) error stop 52
   if ( dtx%var_b /= 8 ) error stop 54
   if ( dtx%var_c /= 12 ) error stop 56
   if ( dtx%var_d /= 16 ) error stop 58
   if ( dtx%var_e /= 20 ) error stop 60
   if ( dtx%var_f /= 24 ) error stop 62
   if ( dtx%var_g /= 28 ) error stop 64
   if ( dtx%var_h /= 32 ) error stop 66

   dty%var_a = dtx%var_a + 1
   dty%var_b = dtx%var_b + 1
   dty%var_c = dtx%var_c + 1
   dty%var_d = dtx%var_d + 1
   dty%var_e = dtx%var_e + 1
   dty%var_f = dtx%var_f + 1
   dty%var_g = dtx%var_g + 1
   dty%var_h = dtx%var_h + 1

end subroutine sub3