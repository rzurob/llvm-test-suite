!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtya00.presh fxdtyia09b cxdtyia09b
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************
!***********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Support for derived types with BIND(C) attr.
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquide
!*  DATE                       : 4/23/2002
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived types with BIND(C) attribute
!*  SECONDARY FUNCTIONS TESTED : see below 
!*
!*  DRIVER STANZA              : 
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
!*      - Testing 2-levels deep derived types with integer components
!*      - Testing FORTRAN functions and C functions
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxdtyia09
   use ISO_C_BINDING

   type, bind(c) :: dt1
      integer(1) :: var_a = 2
      integer(C_INT) :: var_b = 4
      integer(C_SHORT) :: var_c = 6
      integer(8) :: var_d = 8
      integer(C_LONG) :: var_e = 10
      integer(2) :: var_f = 12
      integer(C_LONG_LONG) :: var_g = 14
      integer(4) :: var_h = 16
   end type

   type, bind(c) :: dt2
      integer(C_INT16_T) :: var_a = 2
      integer(C_INT_FAST8_T) :: var_b = 4
      integer(C_INT64_T) :: var_c = 6
      integer(C_INT8_T) :: var_d = 8
      integer(C_INT_FAST32_T) :: var_e = 10
      integer(C_INT_FAST16_T) :: var_f = 12
      integer(C_INT32_T) :: var_g = 14
      integer(C_INT_FAST64_T) :: var_h = 16
      type(dt1) :: vdt1
   end type

end module mxdtyia09

program fxdtyia09b
   use mxdtyia09
   interface
      function fun1(dt) bind(c)
         import dt2
         type(dt2) :: fun1, dt
      end function fun1
   end interface

   type(dt2) :: dta, dtb

!! Test 1

   dtb = fun1(dta)

   if ( dta%var_a /= 3 .or. dtb%var_a /= 3 ) error stop 20
   if ( dta%var_b /= 5 .or. dtb%var_b /= 5 ) error stop 22
   if ( dta%var_c /= 7 .or. dtb%var_c /= 7 ) error stop 24
   if ( dta%var_d /= 9 .or. dtb%var_d /= 9 ) error stop 26
   if ( dta%var_e /= 11 .or. dtb%var_e /= 11 ) error stop 28
   if ( dta%var_f /= 13 .or. dtb%var_f /= 13 ) error stop 30
   if ( dta%var_g /= 15 .or. dtb%var_g /= 15 ) error stop 32
   if ( dta%var_h /= 17 .or. dtb%var_h /= 17 ) error stop 34

   if ( dta%vdt1%var_a /= 4 .or. dtb%vdt1%var_a /= 4 ) error stop 36
   if ( dta%vdt1%var_b /= 6 .or. dtb%vdt1%var_b /= 6 ) error stop 38
   if ( dta%vdt1%var_c /= 8 .or. dtb%vdt1%var_c /= 8 ) error stop 40
   if ( dta%vdt1%var_d /= 10 .or. dtb%vdt1%var_d /= 10 ) error stop 42
   if ( dta%vdt1%var_e /= 12 .or. dtb%vdt1%var_e /= 12 ) error stop 44
   if ( dta%vdt1%var_f /= 14 .or. dtb%vdt1%var_f /= 14 ) error stop 46
   if ( dta%vdt1%var_g /= 16 .or. dtb%vdt1%var_g /= 16 ) error stop 48
   if ( dta%vdt1%var_h /= 18 .or. dtb%vdt1%var_h /= 18 ) error stop 50

end program fxdtyia09b
