!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtya00.presh fxdtyia04 cxdtyia04
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
!*      - Testing 3-levels deep derived types with integer components
!*      - Testing FORTRAN subroutines and C void functions
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtyia04
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
      type(dt1) :: vdt1
      integer(C_INT16_T) var_a
      integer(C_INT_FAST8_T) var_b
      integer(C_INT64_T) var_c
      integer(C_INT8_T) var_d
      integer(C_INT_FAST32_T) var_e
      integer(C_INT_FAST16_T) var_f
      integer(C_INT32_T) var_g
      integer(C_INT_FAST64_T) var_h
   end type

   type, bind(c) :: dt3
      integer(C_SIZE_T) var_a
      integer(C_INT_LEAST8_T) var_b
      integer(C_INT_LEAST64_T) var_c
      integer(C_SIGNED_CHAR) var_d
      integer(C_INTPTR_T) var_e
      integer(C_INT_LEAST16_T) var_f
      integer(C_INTMAX_T) var_g
      integer(C_INT_LEAST32_T) var_h
      type(dt2) :: vdt2
   end type

end module mxdtyia04

module auxmod
   use mxdtyia04

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

program fxdtyia04
   use mxdtyia04
   use auxmod
   interface
      subroutine sub1(dt) bind(c)
         import dt3
         type(dt3) :: dt
      end subroutine sub1
      subroutine sub2(dt) bind(c)
         import dt3
         type(dt3), value :: dt
      end subroutine sub2
      subroutine sub3(dtx,dty) bind(c)
         import dt3
         type(dt3), intent(in) :: dtx
         type(dt3), intent(out) :: dty
      end subroutine sub3
   end interface

   type(dt3) :: dt0 = dt3(2,4,6,8,10,12,14,16, &
                      dt2(dt1(2,4,6,8,10,12,14,16), &
                      2,4,6,8,10,12,14,16))

   type(dt3) :: dta, dtb

!! Test 1

   dta = dt0

   call sub1(dta)

   if ( dta%var_a /= 3 .or. dta%vdt2%var_a /= 4 .or. &
                            dta%vdt2%vdt1%var_a /= 5 ) error stop 20

   if ( dta%var_b /= 5 .or. dta%vdt2%var_b /= 6 .or. &
                            dta%vdt2%vdt1%var_b /= 7 ) error stop 22

   if ( dta%var_c /= 7 .or. dta%vdt2%var_c /= 8 .or. &
                            dta%vdt2%vdt1%var_c /= 9 ) error stop 24

   if ( dta%var_d /= 9 .or. dta%vdt2%var_d /= 10 .or. &
                            dta%vdt2%vdt1%var_d /= 11 ) error stop 26

   if ( dta%var_e /= 11 .or. dta%vdt2%var_e /= 12 .or. &
                             dta%vdt2%vdt1%var_e /= 13 ) error stop 28

   if ( dta%var_f /= 13 .or. dta%vdt2%var_f /= 14 .or. &
                             dta%vdt2%vdt1%var_f /= 15 ) error stop 30

   if ( dta%var_g /= 15 .or. dta%vdt2%var_g /= 16 .or. &
                             dta%vdt2%vdt1%var_g /= 17 ) error stop 32

   if ( dta%var_h /= 17 .or. dta%vdt2%var_h /= 18 .or. &
                             dta%vdt2%vdt1%var_h /= 19 ) error stop 34


!! Test 2

   dta = dt0

   call sub2(dta)

   if ( dta%var_a /= 2 .or. dta%vdt2%var_a /= 2 .or. &
                            dta%vdt2%vdt1%var_a /= 2 ) error stop 36

   if ( dta%var_b /= 4 .or. dta%vdt2%var_b /= 4 .or. &
                            dta%vdt2%vdt1%var_b /= 4 ) error stop 38

   if ( dta%var_c /= 6 .or. dta%vdt2%var_c /= 6 .or. &
                            dta%vdt2%vdt1%var_c /= 6 ) error stop 40

   if ( dta%var_d /= 8 .or. dta%vdt2%var_d /= 8 .or. &
                            dta%vdt2%vdt1%var_d /= 8 ) error stop 42

   if ( dta%var_e /= 10 .or. dta%vdt2%var_e /= 10 .or. &
                             dta%vdt2%vdt1%var_e /= 10 ) error stop 44

   if ( dta%var_f /= 12 .or. dta%vdt2%var_f /= 12 .or. &
                             dta%vdt2%vdt1%var_f /= 12 ) error stop 46

   if ( dta%var_g /= 14 .or. dta%vdt2%var_g /= 14 .or. &
                             dta%vdt2%vdt1%var_g /= 14 ) error stop 48

   if ( dta%var_h /= 16 .or. dta%vdt2%var_h /= 16 .or. &
                             dta%vdt2%vdt1%var_h /= 16 ) error stop 50

!! Test 3

   dta = dt0

   call sub3(dta+dta,dtb)

   if ( dtb%var_a /= 5 .or. dtb%vdt2%var_a /= 6 .or. &
                            dtb%vdt2%vdt1%var_a /= 7 ) error stop 52

   if ( dtb%var_b /= 9 .or. dtb%vdt2%var_b /= 10 .or. &
                            dtb%vdt2%vdt1%var_b /= 11 ) error stop 54

   if ( dtb%var_c /= 13 .or. dtb%vdt2%var_c /= 14 .or. &
                             dtb%vdt2%vdt1%var_c /= 15 ) error stop 56

   if ( dtb%var_d /= 17 .or. dtb%vdt2%var_d /= 18 .or. &
                             dtb%vdt2%vdt1%var_d /= 19 ) error stop 58

   if ( dtb%var_e /= 21 .or. dtb%vdt2%var_e /= 22 .or. &
                             dtb%vdt2%vdt1%var_e /= 23 ) error stop 60

   if ( dtb%var_f /= 25 .or. dtb%vdt2%var_f /= 26 .or. &
                             dtb%vdt2%vdt1%var_f /= 27 ) error stop 62

   if ( dtb%var_g /= 29 .or. dtb%vdt2%var_g /= 30 .or. &
                             dtb%vdt2%vdt1%var_g /= 31 ) error stop 64

   if ( dtb%var_h /= 33 .or. dtb%vdt2%var_h /= 34 .or. &
                             dtb%vdt2%vdt1%var_h /= 35 ) error stop 66

end program fxdtyia04
