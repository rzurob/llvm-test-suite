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
!*      - Main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

function fun1(dt) bind(c)
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
   type(dt2) :: fun1, dt

   if ( dt%var_a /= (2.0e0,2.0e0) .or. dt%vdt1%var_a /= 2 ) error stop 20
   if ( dt%var_b /= 4 .or. dt%vdt1%var_b /= (4.0d0,4.0d0) ) error stop 22
   if ( dt%var_c /= (6.0q0,6.0q0) .or. dt%vdt1%var_c /= 6 ) error stop 24
   if ( dt%var_d /= (8.0d0,8.0d0) .or. dt%vdt1%var_d /= (8.0e0,8.0e0) ) error stop 26
   if ( dt%var_e /= (10.0e0,10.0e0) .or. dt%vdt1%var_e /= (10.0q0,10.0q0) ) error stop 28
   if ( dt%var_f /= 12 .or. dt%vdt1%var_f /= (12.0d0,12.0d0) ) error stop 30
   if ( dt%var_g /= (14.0d0,14.0d0) .or. dt%vdt1%var_g /= 14 ) error stop 32
   if ( dt%var_h /= 16 .or. dt%vdt1%var_h /= (16.0e0,16.0e0) ) error stop 34

   dt%var_a = dt%var_a + (1.0e0,1.0e0)
   dt%var_b = dt%var_b + 1
   dt%var_c = dt%var_c + (1.0q0,1.0q0)
   dt%var_d = dt%var_d + (1.0d0,1.0d0)
   dt%var_e = dt%var_e + (1.0e0,1.0e0)
   dt%var_f = dt%var_f + 1
   dt%var_g = dt%var_g + (1.0d0,1.0d0)
   dt%var_h = dt%var_h + 1

   dt%vdt1%var_a = dt%vdt1%var_a + 2
   dt%vdt1%var_b = dt%vdt1%var_b + (2.0d0,2.0d0)
   dt%vdt1%var_c = dt%vdt1%var_c + 2
   dt%vdt1%var_d = dt%vdt1%var_d + (2.0e0,2.0e0)
   dt%vdt1%var_e = dt%vdt1%var_e + (2.0q0,2.0q0)
   dt%vdt1%var_f = dt%vdt1%var_f + (2.0d0,2.0d0)
   dt%vdt1%var_g = dt%vdt1%var_g + 2
   dt%vdt1%var_h = dt%vdt1%var_h + (2.0e0,2.0e0)

   fun1 = dt

end function fun1

function fun2(dt) bind(c)
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
   type(dt2), value :: dt
   type(dt2) :: fun2

   if ( dt%var_a /= (2.0e0,2.0e0) .or. dt%vdt1%var_a /= 2 ) error stop 36
   if ( dt%var_b /= 4 .or. dt%vdt1%var_b /= (4.0d0,4.0d0) ) error stop 38
   if ( dt%var_c /= (6.0q0,6.0q0) .or. dt%vdt1%var_c /= 6 ) error stop 40
   if ( dt%var_d /= (8.0d0,8.0d0) .or. dt%vdt1%var_d /= (8.0e0,8.0e0) ) error stop 42
   if ( dt%var_e /= (10.0e0,10.0e0) .or. dt%vdt1%var_e /= (10.0q0,10.0q0) ) error stop 44
   if ( dt%var_f /= 12 .or. dt%vdt1%var_f /= (12.0d0,12.0d0) ) error stop 46
   if ( dt%var_g /= (14.0d0,14.0d0) .or. dt%vdt1%var_g /= 14 ) error stop 48
   if ( dt%var_h /= 16 .or. dt%vdt1%var_h /= (16.0e0,16.0e0) ) error stop 50

   dt%var_a = dt%var_a + (1.0e0,1.0e0)
   dt%var_b = dt%var_b + 1
   dt%var_c = dt%var_c + (1.0q0,1.0q0)
   dt%var_d = dt%var_d + (1.0d0,1.0d0)
   dt%var_e = dt%var_e + (1.0e0,1.0e0)
   dt%var_f = dt%var_f + 1
   dt%var_g = dt%var_g + (1.0d0,1.0d0)
   dt%var_h = dt%var_h + 1

   dt%vdt1%var_a = dt%vdt1%var_a + 2
   dt%vdt1%var_b = dt%vdt1%var_b + (2.0d0,2.0d0)
   dt%vdt1%var_c = dt%vdt1%var_c + 2
   dt%vdt1%var_d = dt%vdt1%var_d + (2.0e0,2.0e0)
   dt%vdt1%var_e = dt%vdt1%var_e + (2.0q0,2.0q0)
   dt%vdt1%var_f = dt%vdt1%var_f + (2.0d0,2.0d0)
   dt%vdt1%var_g = dt%vdt1%var_g + 2
   dt%vdt1%var_h = dt%vdt1%var_h + (2.0e0,2.0e0)

   fun2 = dt

end function fun2

function fun3(dtx,dty) bind(c)
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
   type(dt2), intent(in) :: dtx
   type(dt2), intent(out) :: dty
   type(dt2) :: fun3

   if ( dtx%var_a /= (4.0e0,4.0e0) .or. dtx%vdt1%var_a /= 4 ) error stop 52
   if ( dtx%var_b /= 8 .or. dtx%vdt1%var_b /= (8.0d0,8.0d0) ) error stop 54
   if ( dtx%var_c /= (12.0q0,12.0q0) .or. dtx%vdt1%var_c /= 12 ) error stop 56
   if ( dtx%var_d /= (16.0d0,16.0d0) .or. dtx%vdt1%var_d /= (16.0e0,16.0e0) ) error stop 58
   if ( dtx%var_e /= (20.0e0,20.0e0) .or. dtx%vdt1%var_e /= (20.0q0,20.0q0) ) error stop 60
   if ( dtx%var_f /= 24 .or. dtx%vdt1%var_f /= (24.0d0,24.0d0) ) error stop 62
   if ( dtx%var_g /= (28.0d0,28.0d0) .or. dtx%vdt1%var_g /= 28 ) error stop 64
   if ( dtx%var_h /= 32 .or. dtx%vdt1%var_h /= (32.0e0,32.0e0) ) error stop 66

   dty%var_a = dtx%var_a + (1.0e0,1.0e0)
   dty%var_b = dtx%var_b + 1
   dty%var_c = dtx%var_c + (1.0q0,1.0q0)
   dty%var_d = dtx%var_d + (1.0d0,1.0d0)
   dty%var_e = dtx%var_e + (1.0e0,1.0e0)
   dty%var_f = dtx%var_f + 1
   dty%var_g = dtx%var_g + (1.0d0,1.0d0)
   dty%var_h = dtx%var_h + 1

   dty%vdt1%var_a = dtx%vdt1%var_a + 2
   dty%vdt1%var_b = dtx%vdt1%var_b + (2.0d0,2.0d0)
   dty%vdt1%var_c = dtx%vdt1%var_c + 2
   dty%vdt1%var_d = dtx%vdt1%var_d + (2.0e0,2.0e0)
   dty%vdt1%var_e = dtx%vdt1%var_e + (2.0q0,2.0q0)
   dty%vdt1%var_f = dtx%vdt1%var_f + (2.0d0,2.0d0)
   dty%vdt1%var_g = dtx%vdt1%var_g + 2
   dty%vdt1%var_h = dtx%vdt1%var_h + (2.0e0,2.0e0)

   fun3 = dty

end function fun3

function fun4(dtx,dty) bind(c)
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
   type(dt2), intent(in) :: dtx
   type(dt2), intent(out) :: dty
   type(dt2), target, static :: dtz
   type(C_PTR) :: fun4

   if ( dtx%var_a /= (4.0e0,4.0e0) .or. dtx%vdt1%var_a /= 4 ) error stop 68
   if ( dtx%var_b /= 8 .or. dtx%vdt1%var_b /= (8.0d0,8.0d0) ) error stop 70
   if ( dtx%var_c /= (12.0q0,12.0q0) .or. dtx%vdt1%var_c /= 12 ) error stop 72
   if ( dtx%var_d /= (16.0d0,16.0d0) .or. dtx%vdt1%var_d /= (16.0e0,16.0e0) ) error stop 74
   if ( dtx%var_e /= (20.0e0,20.0e0) .or. dtx%vdt1%var_e /= (20.0q0,20.0q0) ) error stop 76
   if ( dtx%var_f /= 24 .or. dtx%vdt1%var_f /= (24.0d0,24.0d0) ) error stop 78
   if ( dtx%var_g /= (28.0d0,28.0d0) .or. dtx%vdt1%var_g /= 28 ) error stop 80
   if ( dtx%var_h /= 32 .or. dtx%vdt1%var_h /= (32.0e0,32.0e0) ) error stop 82

   dtz%var_a = dtx%var_a + (1.0e0,1.0e0)
   dtz%var_b = dtx%var_b + 1
   dtz%var_c = dtx%var_c + (1.0q0,1.0q0)
   dtz%var_d = dtx%var_d + (1.0d0,1.0d0)
   dtz%var_e = dtx%var_e + (1.0e0,1.0e0)
   dtz%var_f = dtx%var_f + 1
   dtz%var_g = dtx%var_g + (1.0d0,1.0d0)
   dtz%var_h = dtx%var_h + 1

   dtz%vdt1%var_a = dtx%vdt1%var_a + 2
   dtz%vdt1%var_b = dtx%vdt1%var_b + (2.0d0,2.0d0)
   dtz%vdt1%var_c = dtx%vdt1%var_c + 2
   dtz%vdt1%var_d = dtx%vdt1%var_d + (2.0e0,2.0e0)
   dtz%vdt1%var_e = dtx%vdt1%var_e + (2.0q0,2.0q0)
   dtz%vdt1%var_f = dtx%vdt1%var_f + (2.0d0,2.0d0)
   dtz%vdt1%var_g = dtx%vdt1%var_g + 2
   dtz%vdt1%var_h = dtx%vdt1%var_h + (2.0e0,2.0e0)

   dty = dtz

   fun4 = C_LOC(dtz)

end function fun4