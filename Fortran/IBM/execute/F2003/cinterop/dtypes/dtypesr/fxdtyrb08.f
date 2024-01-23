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
!*      - Testing FORTRAN subroutines and C void functions
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtyrb08
   use ISO_C_BINDING

   type, bind(c) :: dt1
      integer(C_INT_FAST16_T) var_a
      real(C_DOUBLE) var_b
      integer(C_SIGNED_CHAR) var_c
      real(C_FLOAT) var_d
      real(16) var_e
      real(8) var_f
      integer(C_INTMAX_T) var_g
      real(4) var_h
   end type

   type, bind(c) :: dt2
      real(C_FLOAT) var_a
      integer(C_SHORT) var_b
      real(16) var_c
      real(8) var_d
      real(4) var_e
      integer(1) var_f
      real(C_DOUBLE) var_g
      integer(4) var_h
      type(dt1) :: vdt1
   end type

end module mxdtyrb08

module auxmod
   use mxdtyrb08

   interface operator(+)
      module procedure adddty_s, adddty_d
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

end module auxmod

program fxdtyrb08
   use mxdtyrb08
   use auxmod
   interface
      subroutine sub1(dt) bind(c)
         import dt2
         type(dt2) :: dt
      end subroutine sub1
      subroutine sub2(dt) bind(c)
         import dt2
         type(dt2), value :: dt
      end subroutine sub2
      subroutine sub3(dtx,dty) bind(c)
         import dt2
         type(dt2), intent(in) :: dtx
         type(dt2), intent(out) :: dty
      end subroutine sub3
   end interface

   type(dt2) :: dt0 = dt2(2.0e0,4,6.0q0,8.0d0,10.0e0,12,14.0d0,16, &
                      dt1(2,4.0d0,6,8.0e0,10.0q0,12.0d0,14,16.0e0))

   type(dt2) :: dta, dtb

!! Test 1

   dta = dt0

   call sub1(dta)

   if ( dta%var_a /= 3.0e0 .or. dta%vdt1%var_a /= 4 ) error stop 20
   if ( dta%var_b /= 5 .or. dta%vdt1%var_b /= 6.0d0 ) error stop 22
   if ( dta%var_c /= 7.0q0 .or. dta%vdt1%var_c /= 8 ) error stop 24
   if ( dta%var_d /= 9.0d0 .or. dta%vdt1%var_d /= 10.0e0 ) error stop 26
   if ( dta%var_e /= 11.0e0 .or. dta%vdt1%var_e /= 12.0q0 ) error stop 28
   if ( dta%var_f /= 13 .or. dta%vdt1%var_f /= 14.0d0 ) error stop 30
   if ( dta%var_g /= 15.0d0 .or. dta%vdt1%var_g /= 16 ) error stop 32
   if ( dta%var_h /= 17 .or. dta%vdt1%var_h /= 18.0e0 ) error stop 34

!! Test 2

   dta = dt0

   call sub2(dta)

   if ( dta%var_a /= 2.0e0 .or. dta%vdt1%var_a /= 2 ) error stop 36
   if ( dta%var_b /= 4 .or. dta%vdt1%var_b /= 4.0d0 ) error stop 38
   if ( dta%var_c /= 6.0q0 .or. dta%vdt1%var_c /= 6 ) error stop 40
   if ( dta%var_d /= 8.0d0 .or. dta%vdt1%var_d /= 8.0e0 ) error stop 42
   if ( dta%var_e /= 10.0e0 .or. dta%vdt1%var_e /= 10.0q0 ) error stop 44
   if ( dta%var_f /= 12 .or. dta%vdt1%var_f /= 12.0d0 ) error stop 46
   if ( dta%var_g /= 14.0d0 .or. dta%vdt1%var_g /= 14 ) error stop 48
   if ( dta%var_h /= 16 .or. dta%vdt1%var_h /= 16.0e0 ) error stop 50

!! Test 3

   dta = dt0

   call sub3(dta+dta,dtb)

   if ( dtb%var_a /= 5.0e0 .or. dtb%vdt1%var_a /= 6 ) error stop 52
   if ( dtb%var_b /= 9 .or. dtb%vdt1%var_b /= 10.0d0 ) error stop 54
   if ( dtb%var_c /= 13.0q0 .or. dtb%vdt1%var_c /= 14 ) error stop 56
   if ( dtb%var_d /= 17.0d0 .or. dtb%vdt1%var_d /= 18.0e0 ) error stop 58
   if ( dtb%var_e /= 21.0e0 .or. dtb%vdt1%var_e /= 22.0q0 ) error stop 60
   if ( dtb%var_f /= 25 .or. dtb%vdt1%var_f /= 26.0d0 ) error stop 62
   if ( dtb%var_g /= 29.0d0 .or. dtb%vdt1%var_g /= 30 ) error stop 64
   if ( dtb%var_h /= 33 .or. dtb%vdt1%var_h /= 34.0e0 ) error stop 66

end program fxdtyrb08
