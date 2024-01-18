!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtya01.presh fxdtyrb38
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
!*      - Testing 3-levels deep derived types with BIND(C) attribute
!*      - Testing 3-levels deep derived types with VALUE, INTENT attributes
!*      - Testing 3-levels deep derived types with integer and real components
!*      - Testing FORTRAN module subroutines and host association
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtyrb38
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
      type(dt1) :: vdt1
      real(C_DOUBLE) var_a
      integer(C_INT) var_b
      real(8) var_c
      integer(1) var_d
      real(16) var_e
      real(4) var_f
      integer(C_INT32_T) var_g
      real(C_FLOAT) var_h
   end type

   type, bind(c) :: dt3
      real(C_FLOAT) var_a
      integer(C_SHORT) var_b
      real(16) var_c
      real(8) var_d
      real(4) var_e
      integer(1) var_f
      real(C_DOUBLE) var_g
      integer(4) var_h
      type(dt2) :: vdt2
   end type

   type(dt3) :: dta

contains

subroutine sub1

   if ( dta%var_a /= 2.0e0 .or. dta%vdt2%var_a /= 2.0d0 .or. &
                          dta%vdt2%vdt1%var_a /= 2 ) error stop 20
   if ( dta%var_b /= 4 .or. dta%vdt2%var_b /= 4 .or. &
                          dta%vdt2%vdt1%var_b /= 4.0d0 ) error stop 22
   if ( dta%var_c /= 6.0q0 .or. dta%vdt2%var_c /= 6.0d0 .or. &
                          dta%vdt2%vdt1%var_c /= 6 ) error stop 24
   if ( dta%var_d /= 8.0d0 .or. dta%vdt2%var_d /= 8 .or. &
                          dta%vdt2%vdt1%var_d /= 8.0e0 ) error stop 26
   if ( dta%var_e /= 10.0e0 .or. dta%vdt2%var_e /= 10.0q0 .or. &
                           dta%vdt2%vdt1%var_e /= 10.0q0 ) error stop 28
   if ( dta%var_f /= 12 .or. dta%vdt2%var_f /= 12.0e0 .or. &
                           dta%vdt2%vdt1%var_f /= 12.0d0 ) error stop 30
   if ( dta%var_g /= 14.0d0 .or. dta%vdt2%var_g /= 14 .or. &
                           dta%vdt2%vdt1%var_h /= 16.0e0 ) error stop 32
   if ( dta%var_h /= 16 .or. dta%vdt2%var_h /= 16.0e0 .or. &
                           dta%vdt2%vdt1%var_h /= 16.0e0 ) error stop 34

   dta%var_a = dta%var_a + 1.0e0
   dta%var_b = dta%var_b + 1
   dta%var_c = dta%var_c + 1.0q0
   dta%var_d = dta%var_d + 1.0d0
   dta%var_e = dta%var_e + 1.0e0
   dta%var_f = dta%var_f + 1
   dta%var_g = dta%var_g + 1.0d0
   dta%var_h = dta%var_h + 1

   dta%vdt2%var_a = dta%vdt2%var_a + 2.0d0
   dta%vdt2%var_b = dta%vdt2%var_b + 2
   dta%vdt2%var_c = dta%vdt2%var_c + 2.0d0
   dta%vdt2%var_d = dta%vdt2%var_d + 2
   dta%vdt2%var_e = dta%vdt2%var_e + 2.0q0
   dta%vdt2%var_f = dta%vdt2%var_f + 2.0e0
   dta%vdt2%var_g = dta%vdt2%var_g + 2
   dta%vdt2%var_h = dta%vdt2%var_h + 2.0e0

   dta%vdt2%vdt1%var_a = dta%vdt2%vdt1%var_a + 3
   dta%vdt2%vdt1%var_b = dta%vdt2%vdt1%var_b + 3.0d0
   dta%vdt2%vdt1%var_c = dta%vdt2%vdt1%var_c + 3
   dta%vdt2%vdt1%var_d = dta%vdt2%vdt1%var_d + 3.0e0
   dta%vdt2%vdt1%var_e = dta%vdt2%vdt1%var_e + 3.0q0
   dta%vdt2%vdt1%var_f = dta%vdt2%vdt1%var_f + 3.0d0
   dta%vdt2%vdt1%var_g = dta%vdt2%vdt1%var_g + 3
   dta%vdt2%vdt1%var_h = dta%vdt2%vdt1%var_h + 3.0e0

end subroutine sub1

subroutine sub3(dtu)
   type(dt3), intent(in) :: dtu

   if ( dtu%var_a /= 4.0e0 .or. dtu%vdt2%var_a /= 4.0d0 .or. &
                            dtu%vdt2%vdt1%var_a /= 4 ) error stop 36

   if ( dtu%var_b /= 8 .or. dtu%vdt2%var_b /= 8 .or. &
                            dtu%vdt2%vdt1%var_b /= 8.0d0 ) error stop 38

   if ( dtu%var_c /= 12.0q0 .or. dtu%vdt2%var_c /= 12.0d0 .or. &
                             dtu%vdt2%vdt1%var_c /= 12 ) error stop 40

   if ( dtu%var_d /= 16.0d0 .or. dtu%vdt2%var_d /= 16 .or. &
                             dtu%vdt2%vdt1%var_d /= 16.0e0 ) error stop 42

   if ( dtu%var_e /= 20.0e0 .or. dtu%vdt2%var_e /= 20.0q0 .or. &
                             dtu%vdt2%vdt1%var_e /= 20.0q0 ) error stop 44

   if ( dtu%var_f /= 24 .or. dtu%vdt2%var_f /= 24.0e0 .or. &
                             dtu%vdt2%vdt1%var_f /= 24.0d0 ) error stop 46

   if ( dtu%var_g /= 28.0d0 .or. dtu%vdt2%var_g /= 28 .or. &
                             dtu%vdt2%vdt1%var_g /= 28 ) error stop 48

   if ( dtu%var_h /= 32 .or. dtu%vdt2%var_h /= 32.0e0 .or. &
                             dtu%vdt2%vdt1%var_h /= 32.0e0 ) error stop 50

   dta%var_a = dtu%var_a + 1.0e0
   dta%var_b = dtu%var_b + 1
   dta%var_c = dtu%var_c + 1.0q0
   dta%var_d = dtu%var_d + 1.0d0
   dta%var_e = dtu%var_e + 1.0e0
   dta%var_f = dtu%var_f + 1
   dta%var_g = dtu%var_g + 1.0d0
   dta%var_h = dtu%var_h + 1

   dta%vdt2%var_a = dtu%vdt2%var_a + 2.0d0
   dta%vdt2%var_b = dtu%vdt2%var_b + 2
   dta%vdt2%var_c = dtu%vdt2%var_c + 2.0d0
   dta%vdt2%var_d = dtu%vdt2%var_d + 2
   dta%vdt2%var_e = dtu%vdt2%var_e + 2.0q0
   dta%vdt2%var_f = dtu%vdt2%var_f + 2.0e0
   dta%vdt2%var_g = dtu%vdt2%var_g + 2
   dta%vdt2%var_h = dtu%vdt2%var_h + 2.0e0

   dta%vdt2%vdt1%var_a = dtu%vdt2%vdt1%var_a + 3
   dta%vdt2%vdt1%var_b = dtu%vdt2%vdt1%var_b + 3.0d0
   dta%vdt2%vdt1%var_c = dtu%vdt2%vdt1%var_c + 3
   dta%vdt2%vdt1%var_d = dtu%vdt2%vdt1%var_d + 3.0e0
   dta%vdt2%vdt1%var_e = dtu%vdt2%vdt1%var_e + 3.0q0
   dta%vdt2%vdt1%var_f = dtu%vdt2%vdt1%var_f + 3.0d0
   dta%vdt2%vdt1%var_g = dtu%vdt2%vdt1%var_g + 3
   dta%vdt2%vdt1%var_h = dtu%vdt2%vdt1%var_h + 3.0e0

end subroutine sub3

end module mxdtyrb38

module auxmod
   use mxdtyrb38

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

program fxdtyrb38
   use mxdtyrb38
   use auxmod

   type(dt3) :: dt0 = dt3(2.0e0,4,6.0q0,8.0d0,10.0e0,12,14.0d0,16, &
                      dt2(dt1(2,4.0d0,6,8.0e0,10.0q0,12.0d0,14,16.0e0), &
                      2.0d0,4,6.0d0,8,10.0q0,12.0e0,14,16.0e0))

!! Test 1

   dta = dt0

   call sub1

   if ( dta%var_a /= 3.0e0 .or. dta%vdt2%var_a /= 4.0d0 .or. &
                            dta%vdt2%vdt1%var_a /= 5 ) error stop 52

   if ( dta%var_b /= 5 .or. dta%vdt2%var_b /= 6 .or. &
                            dta%vdt2%vdt1%var_b /= 7.0d0 ) error stop 54

   if ( dta%var_c /= 7.0q0 .or. dta%vdt2%var_c /= 8.0d0 .or. &
                            dta%vdt2%vdt1%var_c /= 9 ) error stop 56

   if ( dta%var_d /= 9.0d0 .or. dta%vdt2%var_d /= 10 .or. &
                            dta%vdt2%vdt1%var_d /= 11.0e0 ) error stop 58

   if ( dta%var_e /= 11.0e0 .or. dta%vdt2%var_e /= 12.0q0 .or. &
                             dta%vdt2%vdt1%var_e /= 13.0q0 ) error stop 60

   if ( dta%var_f /= 13 .or. dta%vdt2%var_f /= 14.0e0 .or. &
                             dta%vdt2%vdt1%var_f /= 15.0d0 ) error stop 62

   if ( dta%var_g /= 15.0d0 .or. dta%vdt2%var_g /= 16 .or. &
                             dta%vdt2%vdt1%var_g /= 17 ) error stop 64

   if ( dta%var_h /= 17 .or. dta%vdt2%var_h /= 18.0e0 .or. &
                             dta%vdt2%vdt1%var_h /= 19.0e0 ) error stop 66


!! Test 3

   dta = dt0

   call sub3(dta+dta)

   if ( dta%var_a /= 5.0e0 .or. dta%vdt2%var_a /= 6.0d0 .or. &
                            dta%vdt2%vdt1%var_a /= 7 ) error stop 68

   if ( dta%var_b /= 9 .or. dta%vdt2%var_b /= 10 .or. &
                            dta%vdt2%vdt1%var_b /= 11.0d0 ) error stop 70

   if ( dta%var_c /= 13.0q0 .or. dta%vdt2%var_c /= 14.0d0 .or. &
                             dta%vdt2%vdt1%var_c /= 15 ) error stop 72

   if ( dta%var_d /= 17.0d0 .or. dta%vdt2%var_d /= 18 .or. &
                             dta%vdt2%vdt1%var_d /= 19.0e0 ) error stop 74

   if ( dta%var_e /= 21.0e0 .or. dta%vdt2%var_e /= 22.0q0 .or. &
                             dta%vdt2%vdt1%var_e /= 23.0q0 ) error stop 76

   if ( dta%var_f /= 25 .or. dta%vdt2%var_f /= 26.0e0 .or. &
                             dta%vdt2%vdt1%var_f /= 27.0d0 ) error stop 78

   if ( dta%var_g /= 29.0d0 .or. dta%vdt2%var_g /= 30 .or. &
                             dta%vdt2%vdt1%var_g /= 31 ) error stop 80

   if ( dta%var_h /= 33 .or. dta%vdt2%var_h /= 34.0e0 .or. &
                             dta%vdt2%vdt1%var_h /= 35.0e0 ) error stop 82

end program fxdtyrb38
