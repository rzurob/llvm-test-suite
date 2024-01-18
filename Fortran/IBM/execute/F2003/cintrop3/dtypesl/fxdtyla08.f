!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtye00.presh fxdtyla08 cxdtyla08
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
!*      - Testing 2-levels deep derived types with logical, character and real components
!*      - Testing FORTRAN subroutines and C void functions
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtyla08
   use ISO_C_BINDING

   type, bind(c) :: dt1
      logical(C_BOOL) var_a
      real(C_DOUBLE) var_b
      character(C_CHAR) var_c
      real(C_FLOAT) var_d
      real(C_LONG_DOUBLE) var_e
      real(8) var_f
      logical(C_BOOL) var_g
      real(4) var_h
   end type

   type, bind(c) :: dt2
      real(C_FLOAT) var_a
      logical(C_BOOL) var_b
      real(C_LONG_DOUBLE) var_c
      real(8) var_d
      real(4) var_e
      logical(C_BOOL) var_f
      real(C_DOUBLE) var_g
      character(C_CHAR) var_h
      type(dt1) :: vdt1
   end type

end module mxdtyla08

module auxmod
   use mxdtyla08

   interface operator(+)
      module procedure adddty_s, adddty_d
   end interface

contains
   function adddty_s(dtx,dty)
      type(dt1), intent(in) :: dtx, dty
      type(dt1) :: adddty_s

      adddty_s%var_a = dtx%var_a .and. dty%var_a
      adddty_s%var_b = dtx%var_b + dty%var_b
      adddty_s%var_c = dtx%var_c
      adddty_s%var_d = dtx%var_d + dty%var_d
      adddty_s%var_e = dtx%var_e + dty%var_e
      adddty_s%var_f = dtx%var_f + dty%var_f
      adddty_s%var_g = dtx%var_g .and. dty%var_g
      adddty_s%var_h = dtx%var_h + dty%var_h

   end function adddty_s

   function adddty_d(dtx,dty)
      type(dt2), intent(in) :: dtx, dty
      type(dt2) :: adddty_d

      adddty_d%var_a = dtx%var_a + dty%var_a
      adddty_d%var_b = dtx%var_b .and. dty%var_b
      adddty_d%var_c = dtx%var_c + dty%var_c
      adddty_d%var_d = dtx%var_d + dty%var_d
      adddty_d%var_e = dtx%var_e + dty%var_e
      adddty_d%var_f = dtx%var_f .and. dty%var_f
      adddty_d%var_g = dtx%var_g + dty%var_g
      adddty_d%var_h = dtx%var_h

      adddty_d%vdt1 = adddty_s(dtx%vdt1,dty%vdt1)

   end function adddty_d

end module auxmod

program fxdtyla08
   use mxdtyla08
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

   type(dt2) :: dt0 = dt2(2.0e0,.true.,6.0q0,8.0d0,10.0e0,.true.,14.0d0,'A', &
                      dt1(.true.,4.0d0,'A',8.0e0,10.0q0,12.0d0,.true.,16.0e0))

   type(dt2) :: dta, dtb

!! Test 1

   dta = dt0

   call sub1(dta)

   if ( dta%var_a /= 3.0e0 .or. (dta%vdt1%var_a .neqv. .false.) ) error stop 20
   if ( (dta%var_b .neqv. .false.) .or. dta%vdt1%var_b /= 6 ) error stop 22
   if ( dta%var_c /= 7.0q0 .or. (dta%vdt1%var_c /= 'B') ) error stop 24
   if ( dta%var_d /= 9.0d0 .or. dta%vdt1%var_d /= 10 ) error stop 26
   if ( dta%var_e /= 11.0e0 .or. dta%vdt1%var_e /= 12 ) error stop 28
   if ( (dta%var_f .neqv. .false.) .or. dta%vdt1%var_f /= 14 ) error stop 30
   if ( dta%var_g /= 15.0d0 .or. (dta%vdt1%var_g .neqv. .false.) ) error stop 32
   if ( (dta%var_h /= 'B') .or. dta%vdt1%var_h /= 18 ) error stop 34

!! Test 2

   dta = dt0

   call sub2(dta)

   if ( dta%var_a /= 2.0e0 .or. (dta%vdt1%var_a .neqv. .true.) ) error stop 36
   if ( (dta%var_b .neqv. .true.) .or. dta%vdt1%var_b /= 4 ) error stop 38
   if ( dta%var_c /= 6.0q0 .or. (dta%vdt1%var_c /= 'A') ) error stop 40
   if ( dta%var_d /= 8.0d0 .or. dta%vdt1%var_d /= 8 ) error stop 42
   if ( dta%var_e /= 10.0e0 .or. dta%vdt1%var_e /= 10 ) error stop 44
   if ( (dta%var_f .neqv. .true.) .or. dta%vdt1%var_f /= 12 ) error stop 46
   if ( dta%var_g /= 14.0d0 .or. (dta%vdt1%var_g .neqv. .true.) ) error stop 48
   if ( (dta%var_h /= 'A') .or. dta%vdt1%var_h /= 16 ) error stop 50

!! Test 3

   dta = dt0

   call sub3(dta+dta,dtb)

   if ( dtb%var_a /= 5.0e0 .or. (dtb%vdt1%var_a .neqv. .false.) ) error stop 52
   if ( (dtb%var_b .neqv. .false.) .or. dtb%vdt1%var_b /= 10 ) error stop 54
   if ( dtb%var_c /= 13.0q0 .or. (dtb%vdt1%var_c /= 'B') ) error stop 56
   if ( dtb%var_d /= 17.0d0 .or. dtb%vdt1%var_d /= 18 ) error stop 58
   if ( dtb%var_e /= 21.0e0 .or. dtb%vdt1%var_e /= 22 ) error stop 60
   if ( (dtb%var_f .neqv. .false.) .or. dtb%vdt1%var_f /= 26 ) error stop 62
   if ( dtb%var_g /= 29.0d0 .or. (dtb%vdt1%var_g .neqv. .false.) ) error stop 64
   if ( (dtb%var_h /= 'B') .or. dtb%vdt1%var_h /= 34 ) error stop 66

end program fxdtyla08
