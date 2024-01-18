!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtya00.presh fxdtyia00 cxdtyia00
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
!*      - Testing single derived types with BIND(C) attribute
!*      - Testing single derived types with VALUE, INTENT attributes
!*      - Testing single derived types with integer components
!*      - Testing FORTRAN subroutines and C void functions
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxdtyia00
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

end module mxdtyia00

module auxmod
   use mxdtyia00

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
end module auxmod

program abstractif056
   use mxdtyia00
   use auxmod
   abstract interface
      subroutine sub1ai(dt) bind(c)
         import dt1
         type(dt1) :: dt
      end subroutine sub1ai
      subroutine sub2ai(dt) bind(c)
         import dt1
         type(dt1), value :: dt
      end subroutine sub2ai
      subroutine sub3ai(dtx,dty) bind(c)
         import dt1
         type(dt1), intent(in) :: dtx
         type(dt1), intent(out) :: dty
      end subroutine sub3ai
   end interface

   procedure (sub1ai) :: sub1
   procedure (sub2ai) :: sub2
   procedure (sub3ai) :: sub3

   type(dt1) :: dt0 = dt1(2,4,6,8,10,12,14,16)

   type(dt1) :: dta, dtb

!! Test 1

   dta = dt0

   call sub1(dta)

   if ( dta%var_a /= 3 ) error stop 20
   if ( dta%var_b /= 5 ) error stop 22
   if ( dta%var_c /= 7 ) error stop 24
   if ( dta%var_d /= 9 ) error stop 26
   if ( dta%var_e /= 11 ) error stop 28
   if ( dta%var_f /= 13 ) error stop 30
   if ( dta%var_g /= 15 ) error stop 32
   if ( dta%var_h /= 17 ) error stop 34

!! Test 2

   dta = dt0

   call sub2(dta)

   if ( dta%var_a /= 2 ) error stop 36
   if ( dta%var_b /= 4 ) error stop 38
   if ( dta%var_c /= 6 ) error stop 40
   if ( dta%var_d /= 8 ) error stop 42
   if ( dta%var_e /= 10 ) error stop 44
   if ( dta%var_f /= 12 ) error stop 46
   if ( dta%var_g /= 14 ) error stop 48
   if ( dta%var_h /= 16 ) error stop 50

!! Test 3

   dta = dt0

   call sub3(dta+dta,dtb)

   if ( dtb%var_a /= 5 ) error stop 52
   if ( dtb%var_b /= 9 ) error stop 54
   if ( dtb%var_c /= 13 ) error stop 56
   if ( dtb%var_d /= 17 ) error stop 58
   if ( dtb%var_e /= 21 ) error stop 60
   if ( dtb%var_f /= 25 ) error stop 62
   if ( dtb%var_g /= 29 ) error stop 64
   if ( dtb%var_h /= 33 ) error stop 66

end program abstractif056
