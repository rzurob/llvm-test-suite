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
!*      - Testing single derived types with integer and real components
!*      - Testing FORTRAN subroutines and C void functions
!*      - Main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine sub1(dt) bind(c)
   use ISO_C_BINDING
   type, bind(c) :: dt1
      complex(C_FLOAT_COMPLEX) var_a
      integer(C_SHORT) var_b
      complex(16) var_c
      complex(8) var_d
      complex(4) var_e
      integer(1) var_f
      complex(C_DOUBLE_COMPLEX) var_g
      integer(4) var_h
   end type
   type(dt1) :: dt

   if ( dt%var_a /= (2.0e0,2.0e0) ) error stop 20
   if ( dt%var_b /= 4 ) error stop 22
   if ( dt%var_c /= (6.0q0,6.0q0) ) error stop 24
   if ( dt%var_d /= (8.0d0,8.0d0) ) error stop 26
   if ( dt%var_e /= (10.0e0,10.0e0) ) error stop 28
   if ( dt%var_f /= 12 ) error stop 30
   if ( dt%var_g /= (14.0d0,14.0d0) ) error stop 32
   if ( dt%var_h /= 16 ) error stop 34

   dt%var_a = dt%var_a + (1.0e0,1.0e0)
   dt%var_b = dt%var_b + 1
   dt%var_c = dt%var_c + (1.0q0,1.0q0)
   dt%var_d = dt%var_d + (1.0d0,1.0d0)
   dt%var_e = dt%var_e + (1.0e0,1.0e0)
   dt%var_f = dt%var_f + 1
   dt%var_g = dt%var_g + (1.0d0,1.0d0)
   dt%var_h = dt%var_h + 1

end subroutine sub1

subroutine sub2(dt) bind(c)
   use ISO_C_BINDING
   type, bind(c) :: dt1
      complex(C_FLOAT_COMPLEX) var_a
      integer(C_SHORT) var_b
      complex(16) var_c
      complex(8) var_d
      complex(4) var_e
      integer(1) var_f
      complex(C_DOUBLE_COMPLEX) var_g
      integer(4) var_h
   end type
   type(dt1), value :: dt

   if ( dt%var_a /= (2.0e0,2.0e0) ) error stop 36
   if ( dt%var_b /= 4 ) error stop 38
   if ( dt%var_c /= (6.0q0,6.0q0) ) error stop 40
   if ( dt%var_d /= (8.0d0,8.0d0) ) error stop 42
   if ( dt%var_e /= (10.0e0,10.0e0) ) error stop 44
   if ( dt%var_f /= 12 ) error stop 46
   if ( dt%var_g /= (14.0d0,14.0d0) ) error stop 48
   if ( dt%var_h /= 16 ) error stop 50

   dt%var_a = dt%var_a + (1.0e0,1.0e0)
   dt%var_b = dt%var_b + 1
   dt%var_c = dt%var_c + (1.0q0,1.0q0)
   dt%var_d = dt%var_d + (1.0d0,1.0d0)
   dt%var_e = dt%var_e + (1.0e0,1.0e0)
   dt%var_f = dt%var_f + 1
   dt%var_g = dt%var_g + (1.0d0,1.0d0)
   dt%var_h = dt%var_h + 1

end subroutine sub2

subroutine sub3(dtx,dty) bind(c)
   use ISO_C_BINDING
   type, bind(c) :: dt1
      complex(C_FLOAT_COMPLEX) var_a
      integer(C_SHORT) var_b
      complex(16) var_c
      complex(8) var_d
      complex(4) var_e
      integer(1) var_f
      complex(C_DOUBLE_COMPLEX) var_g
      integer(4) var_h
   end type
   type(dt1), intent(in) :: dtx
   type(dt1), intent(out) :: dty

   if ( dtx%var_a /= (4.0e0,4.0e0) ) error stop 52
   if ( dtx%var_b /= 8 ) error stop 54
   if ( dtx%var_c /= (12.0q0,12.0q0) ) error stop 56
   if ( dtx%var_d /= (16.0d0,16.0d0) ) error stop 58
   if ( dtx%var_e /= (20.0e0,20.0e0) ) error stop 60
   if ( dtx%var_f /= 24 ) error stop 62
   if ( dtx%var_g /= (28.0d0,28.0d0) ) error stop 64
   if ( dtx%var_h /= 32 ) error stop 66

   dty%var_a = dtx%var_a + (1.0e0,1.0e0)
   dty%var_b = dtx%var_b + 1
   dty%var_c = dtx%var_c + (1.0q0,1.0q0)
   dty%var_d = dtx%var_d + (1.0d0,1.0d0)
   dty%var_e = dtx%var_e + (1.0e0,1.0e0)
   dty%var_f = dtx%var_f + 1
   dty%var_g = dtx%var_g + (1.0d0,1.0d0)
   dty%var_h = dtx%var_h + 1

end subroutine sub3