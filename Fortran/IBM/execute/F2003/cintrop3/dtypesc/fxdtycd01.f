!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtyd00.presh fxdtycd01 cxdtycc01
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
!*      - Testing single derived types with VALUE, INTENT attributes.
!*      - Testing single derived types with integer and real components
!*      - Testing FORTRAN functions and C functions
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxdtycd01
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

end module mxdtycd01

module modop
   use mxdtycd01

   interface operator(+)
      module procedure adddty
   end interface

contains
   function adddty(dtx,dty)
      type(dt1), intent(in) :: dtx, dty
      type(dt1) :: adddty

      adddty%var_a = dtx%var_a + dty%var_a
      adddty%var_b = dtx%var_b + dty%var_b
      adddty%var_c = dtx%var_c + dty%var_c
      adddty%var_d = dtx%var_d + dty%var_d
      adddty%var_e = dtx%var_e + dty%var_e
      adddty%var_f = dtx%var_f + dty%var_f
      adddty%var_g = dtx%var_g + dty%var_g
      adddty%var_h = dtx%var_h + dty%var_h

   end function adddty
end module modop

program fxdtycd01
   use ISO_C_BINDING, ONLY : C_PTR, C_F_POINTER
   use mxdtycd01
   use modop
   interface
      function fun1(dt) bind(c)
         import dt1
         type(dt1) :: fun1, dt
      end function fun1
      function fun2(dt) bind(c)
         import dt1
         type(dt1), value :: dt
         type(dt1) :: fun2
      end function fun2
      function fun3(dtx,dty) bind(c)
         import dt1
         type(dt1), intent(in) :: dtx
         type(dt1), intent(out) :: dty
         type(dt1) :: fun3
      end function fun3
      function fun4(dtx,dty) bind(c)
         import dt1, C_PTR
         type(dt1), intent(in) :: dtx
         type(dt1), intent(out) :: dty
         type(C_PTR) :: fun4
      end function fun4
   end interface

   type(dt1) :: dt0 = dt1((2.0e0,2.0e0),4,(6.0q0,6.0q0),(8.0d0,8.0d0),(10.0e0,10.0e0),12,(14.0d0,14.0d0),16)

   type(dt1) :: dta, dtb, dtc
   type(dt1), pointer :: dtd
   type(C_PTR) :: dtp

!! Test 1

   dta = dt0

   dtb = fun1(dta)

   if ( dta%var_a /= (3.0e0,3.0e0) .or. dtb%var_a /= (3.0e0,3.0e0) ) error stop 20
   if ( dta%var_b /= 5 .or. dtb%var_b /= 5 ) error stop 22
   if ( dta%var_c /= (7.0q0,7.0q0) .or. dtb%var_c /= (7.0q0,7.0q0) ) error stop 24
   if ( dta%var_d /= (9.0d0,9.0d0) .or. dtb%var_d /= (9.0d0,9.0d0) ) error stop 26
   if ( dta%var_e /= (11.0e0,11.0e0) .or. dtb%var_e /= (11.0e0,11.0e0) ) error stop 28
   if ( dta%var_f /= 13 .or. dtb%var_f /= 13 ) error stop 30
   if ( dta%var_g /= (15.0d0,15.0d0) .or. dtb%var_g /= (15.0d0,15.0d0) ) error stop 32
   if ( dta%var_h /= 17 .or. dtb%var_h /= 17 ) error stop 34

!! Test 2

   dta = dt0

   dtb = fun2(dta)

   if ( dta%var_a /= (2.0e0,2.0e0) .or. dtb%var_a /= (3.0e0,3.0e0) ) error stop 36
   if ( dta%var_b /= 4 .or. dtb%var_b /= 5 ) error stop 38
   if ( dta%var_c /= (6.0q0,6.0q0) .or. dtb%var_c /= (7.0q0,7.0q0) ) error stop 40
   if ( dta%var_d /= (8.0d0,8.0d0) .or. dtb%var_d /= (9.0d0,9.0d0) ) error stop 42
   if ( dta%var_e /= (10.0e0,10.0e0) .or. dtb%var_e /= (11.0e0,11.0e0) ) error stop 44
   if ( dta%var_f /= 12 .or. dtb%var_f /= 13 ) error stop 46
   if ( dta%var_g /= (14.0d0,14.0d0) .or. dtb%var_g /= (15.0d0,15.0d0) ) error stop 48
   if ( dta%var_h /= 16 .or. dtb%var_h /= 17 ) error stop 50

!! Test 3

   dta = dt0

   dtc = fun3(dta+dta,dtb)

   if ( dtb%var_a /= (5.0e0,5.0e0) .or. dtc%var_a /= (5.0e0,5.0e0) ) error stop 52
   if ( dtb%var_b /= 9 .or. dtc%var_b /= 9 ) error stop 54
   if ( dtb%var_c /= (13.0q0,13.0q0) .or. dtc%var_c /= (13.0q0,13.0q0) ) error stop 56
   if ( dtb%var_d /= (17.0d0,17.0d0) .or. dtc%var_d /= (17.0d0,17.0d0) ) error stop 58
   if ( dtb%var_e /= (21.0e0,21.0e0) .or. dtc%var_e /= (21.0e0,21.0e0) ) error stop 60
   if ( dtb%var_f /= 25 .or. dtc%var_f /= 25 ) error stop 62
   if ( dtb%var_g /= (29.0d0,29.0d0) .or. dtc%var_g /= (29.0d0,29.0d0) ) error stop 64
   if ( dtb%var_h /= 33 .or. dtc%var_h /= 33 ) error stop 66

!! Test 4

   dta = dt0

   dtp = fun4(dta+dta,dtb)

   call C_F_POINTER(dtp,dtd)

   if ( dtb%var_a /= (5.0e0,5.0e0) .or. dtd%var_a /= (6.0e0,6.0e0) ) error stop 68
   if ( dtb%var_b /= 9 .or. dtd%var_b /= 10 ) error stop 70
   if ( dtb%var_c /= (13.0q0,13.0q0) .or. dtd%var_c /= (14.0q0,14.0q0) ) error stop 72
   if ( dtb%var_d /= (17.0d0,17.0d0) .or. dtd%var_d /= (18.0d0,18.0d0) ) error stop 74
   if ( dtb%var_e /= (21.0e0,21.0e0) .or. dtd%var_e /= (22.0e0,22.0e0) ) error stop 76
   if ( dtb%var_f /= 25 .or. dtd%var_f /= 26 ) error stop 78
   if ( dtb%var_g /= (29.0d0,29.0d0) .or. dtd%var_g /= (30.0d0,30.0d0) ) error stop 80
   if ( dtb%var_h /= 33 .or. dtd%var_h /= 34 ) error stop 82

end program fxdtycd01
