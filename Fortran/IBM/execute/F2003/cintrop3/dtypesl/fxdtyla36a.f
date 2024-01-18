!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtya01.presh fxdtyla36a
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
!*      - Testing 3-levels deep derived types with BIND(C) attribute
!*      - Testing 3-levels deep derived types with VALUE, INTENT attributes
!*      - Testing 3-levels deep derived types with logical, character and real components
!*      - Testing FORTRAN external subroutines
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxdtyla36
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
      type(dt1) :: vdt1
      real(C_DOUBLE) var_a
      logical(C_BOOL) var_b
      real(8) var_c
      character(C_CHAR) var_d
      real(C_LONG_DOUBLE) var_e
      real(4) var_f
      logical(C_BOOL) var_g
      real(C_FLOAT) var_h
   end type

   type, bind(c) :: dt3
      real(C_FLOAT) var_a
      logical(C_BOOL) var_b
      real(C_LONG_DOUBLE) var_c
      real(8) var_d
      real(4) var_e
      logical(C_BOOL) var_f
      real(C_DOUBLE) var_g
      character(C_CHAR) var_h
      type(dt2) :: vdt2
   end type

end module mxdtyla36

module auxmod
   use mxdtyla36

   interface operator(+)
      module procedure adddty_s, adddty_d, adddty_t
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
      adddty_d%var_d = dtx%var_d
      adddty_d%var_e = dtx%var_e + dty%var_e
      adddty_d%var_f = dtx%var_f + dty%var_f
      adddty_d%var_g = dtx%var_g .and. dty%var_g
      adddty_d%var_h = dtx%var_h + dty%var_h

      adddty_d%vdt1 = adddty_s(dtx%vdt1,dty%vdt1)

   end function adddty_d

   function adddty_t(dtx,dty)
      type(dt3), intent(in) :: dtx, dty
      type(dt3) :: adddty_t

      adddty_t%var_a = dtx%var_a + dty%var_a
      adddty_t%var_b = dtx%var_b .and. dty%var_b
      adddty_t%var_c = dtx%var_c + dty%var_c
      adddty_t%var_d = dtx%var_d + dty%var_d
      adddty_t%var_e = dtx%var_e + dty%var_e
      adddty_t%var_f = dtx%var_f .and. dty%var_f
      adddty_t%var_g = dtx%var_g + dty%var_g
      adddty_t%var_h = dtx%var_h

      adddty_t%vdt2 = adddty_d(dtx%vdt2,dty%vdt2)

   end function adddty_t

end module auxmod

program fxdtyla36a
   use mxdtyla36
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

   type(dt3) :: dt0 = dt3(2.0e0,.true.,6.0q0,8.0d0,10.0e0,.true.,14.0d0,'A', &
                      dt2(dt1(.true.,4.0d0,'A',8.0e0,10.0q0,12.0d0,.true.,16.0e0), &
                      2.0d0,.true.,6.0d0,'A',10.0q0,12.0e0,.true.,16.0e0))

   type(dt3) :: dta, dtb

!! Test 1

   dta = dt0

   call sub1(dta)

   if ( dta%var_a /= 3.0e0 .or. dta%vdt2%var_a /= 4.0d0 .or. &
                            (dta%vdt2%vdt1%var_a .neqv. .false.) ) error stop 20

   if ( (dta%var_b .neqv. .false.) .or. (dta%vdt2%var_b .neqv. .false.) .or. &
                            dta%vdt2%vdt1%var_b /= 7.0d0 ) error stop 22

   if ( dta%var_c /= 7.0q0 .or. dta%vdt2%var_c /= 8.0d0 .or. &
                            (dta%vdt2%vdt1%var_c /= 'B') ) error stop 24

   if ( dta%var_d /= 9.0d0 .or. (dta%vdt2%var_d /= 'B') .or. &
                            dta%vdt2%vdt1%var_d /= 11.0e0 ) error stop 26

   if ( dta%var_e /= 11.0e0 .or. dta%vdt2%var_e /= 12.0q0 .or. &
                             dta%vdt2%vdt1%var_e /= 13.0q0 ) error stop 28

   if ( (dta%var_f .neqv. .false.) .or. dta%vdt2%var_f /= 14.0e0 .or. &
                             dta%vdt2%vdt1%var_f /= 15.0d0 ) error stop 30

   if ( dta%var_g /= 15.0d0 .or. (dta%vdt2%var_g .neqv. .false.) .or. &
                             (dta%vdt2%vdt1%var_g .neqv. .false.) ) error stop 32

   if ( (dta%var_h /= 'B') .or. dta%vdt2%var_h /= 18.0e0 .or. &
                             dta%vdt2%vdt1%var_h /= 19.0e0 ) error stop 34


!! Test 2

   dta = dt0

   call sub2(dta)

   if ( dta%var_a /= 2.0e0 .or. dta%vdt2%var_a /= 2.0d0 .or. &
                            (dta%vdt2%vdt1%var_a .neqv. .true.) ) error stop 36

   if ( (dta%var_b .neqv. .true.) .or. (dta%vdt2%var_b .neqv. .true.) .or. &
                            dta%vdt2%vdt1%var_b /= 4.0d0 ) error stop 38

   if ( dta%var_c /= 6.0q0 .or. dta%vdt2%var_c /= 6.0d0 .or. &
                            (dta%vdt2%vdt1%var_c /= 'A') ) error stop 40

   if ( dta%var_d /= 8.0d0 .or. (dta%vdt2%var_d /= 'A') .or. &
                            dta%vdt2%vdt1%var_d /= 8.0e0 ) error stop 42

   if ( dta%var_e /= 10.0e0 .or. dta%vdt2%var_e /= 10.0q0 .or. &
                             dta%vdt2%vdt1%var_e /= 10.0q0 ) error stop 44

   if ( (dta%var_f .neqv. .true.) .or. dta%vdt2%var_f /= 12.0e0 .or. &
                             dta%vdt2%vdt1%var_f /= 12.0d0 ) error stop 46

   if ( dta%var_g /= 14.0d0 .or. (dta%vdt2%var_g .neqv. .true.) .or. &
                             (dta%vdt2%vdt1%var_g .neqv. .true.) ) error stop 48

   if ( (dta%var_h /= 'A') .or. dta%vdt2%var_h /= 16.0e0 .or. &
                             dta%vdt2%vdt1%var_h /= 16.0e0 ) error stop 50

!! Test 3

   dta = dt0

   call sub3(dta+dta,dtb)

   if ( dtb%var_a /= 5.0e0 .or. dtb%vdt2%var_a /= 6.0d0 .or. &
                            (dtb%vdt2%vdt1%var_a .neqv. .false.) ) error stop 52

   if ( (dtb%var_b .neqv. .false.) .or. (dtb%vdt2%var_b .neqv. .false.) .or. &
                            dtb%vdt2%vdt1%var_b /= 11.0d0 ) error stop 54

   if ( dtb%var_c /= 13.0q0 .or. dtb%vdt2%var_c /= 14.0d0 .or. &
                             (dtb%vdt2%vdt1%var_c /= 'B') ) error stop 56

   if ( dtb%var_d /= 17.0d0 .or. (dtb%vdt2%var_d /= 'B') .or. &
                             dtb%vdt2%vdt1%var_d /= 19.0e0 ) error stop 58

   if ( dtb%var_e /= 21.0e0 .or. dtb%vdt2%var_e /= 22.0q0 .or. &
                             dtb%vdt2%vdt1%var_e /= 23.0q0 ) error stop 60

   if ( (dtb%var_f .neqv. .false.) .or. dtb%vdt2%var_f /= 26.0e0 .or. &
                             dtb%vdt2%vdt1%var_f /= 27.0d0 ) error stop 62

   if ( dtb%var_g /= 29.0d0 .or. (dtb%vdt2%var_g .neqv. .false.) .or. &
                             (dtb%vdt2%vdt1%var_g .neqv. .false.) ) error stop 64

   if ( (dtb%var_h /= 'B') .or. dtb%vdt2%var_h /= 34.0e0 .or. &
                             dtb%vdt2%vdt1%var_h /= 35.0e0 ) error stop 66

end program fxdtyla36a

!! Test 1 Sub
subroutine sub1(dtk) bind(c)
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
      type(dt1) :: vdt1
      real(C_DOUBLE) var_a
      logical(C_BOOL) var_b
      real(8) var_c
      character(C_CHAR) var_d
      real(C_LONG_DOUBLE) var_e
      real(4) var_f
      logical(C_BOOL) var_g
      real(C_FLOAT) var_h
   end type

   type, bind(c) :: dt3
      real(C_FLOAT) var_a
      logical(C_BOOL) var_b
      real(C_LONG_DOUBLE) var_c
      real(8) var_d
      real(4) var_e
      logical(C_BOOL) var_f
      real(C_DOUBLE) var_g
      character(C_CHAR) var_h
      type(dt2) :: vdt2
   end type

   type(dt3) :: dtk

   if ( dtk%var_a /= 2.0e0 .or. dtk%vdt2%var_a /= 2.0d0 .or. & 
                          (dtk%vdt2%vdt1%var_a .neqv. .true.) ) error stop 68
   if ( (dtk%var_b .neqv. .true.) .or. (dtk%vdt2%var_b .neqv. .true.) .or. & 
                          dtk%vdt2%vdt1%var_b /= 4.0d0 ) error stop 70
   if ( dtk%var_c /= 6.0q0 .or. dtk%vdt2%var_c /= 6.0d0 .or. &
                          (dtk%vdt2%vdt1%var_c /= 'A') ) error stop 72
   if ( dtk%var_d /= 8.0d0 .or. (dtk%vdt2%var_d /= 'A') .or. &
                          dtk%vdt2%vdt1%var_d /= 8.0e0 ) error stop 74
   if ( dtk%var_e /= 10.0e0 .or. dtk%vdt2%var_e /= 10.0q0 .or. &
                           dtk%vdt2%vdt1%var_e /= 10.0q0 ) error stop 76
   if ( (dtk%var_f .neqv. .true.) .or. dtk%vdt2%var_f /= 12.0e0 .or. &
                           dtk%vdt2%vdt1%var_f /= 12.0d0 ) error stop 78
   if ( dtk%var_g /= 14.0d0 .or. (dtk%vdt2%var_g .neqv. .true.) .or. &
                           dtk%vdt2%vdt1%var_h /= 16.0e0 ) error stop 80
   if ( (dtk%var_h /= 'A') .or. dtk%vdt2%var_h /= 16.0e0 .or. &
                           dtk%vdt2%vdt1%var_h /= 16.0e0 ) error stop 82

   dtk%var_a = dtk%var_a + 1.0e0
   dtk%var_b = .not. dtk%var_b
   dtk%var_c = dtk%var_c + 1.0q0
   dtk%var_d = dtk%var_d + 1.0d0
   dtk%var_e = dtk%var_e + 1.0e0
   dtk%var_f = .not. dtk%var_f
   dtk%var_g = dtk%var_g + 1.0d0
   dtk%var_h = 'B'

   dtk%vdt2%var_a = dtk%vdt2%var_a + 2.0d0
   dtk%vdt2%var_b = .not. dtk%vdt2%var_b
   dtk%vdt2%var_c = dtk%vdt2%var_c + 2.0d0
   dtk%vdt2%var_d = 'B'
   dtk%vdt2%var_e = dtk%vdt2%var_e + 2.0q0
   dtk%vdt2%var_f = dtk%vdt2%var_f + 2.0e0
   dtk%vdt2%var_g = .not. dtk%vdt2%var_g
   dtk%vdt2%var_h = dtk%vdt2%var_h + 2.0e0

   dtk%vdt2%vdt1%var_a = .not. dtk%vdt2%vdt1%var_a
   dtk%vdt2%vdt1%var_b = dtk%vdt2%vdt1%var_b + 3.0d0
   dtk%vdt2%vdt1%var_c = 'B'
   dtk%vdt2%vdt1%var_d = dtk%vdt2%vdt1%var_d + 3.0e0
   dtk%vdt2%vdt1%var_e = dtk%vdt2%vdt1%var_e + 3.0q0
   dtk%vdt2%vdt1%var_f = dtk%vdt2%vdt1%var_f + 3.0d0
   dtk%vdt2%vdt1%var_g = .not. dtk%vdt2%vdt1%var_g
   dtk%vdt2%vdt1%var_h = dtk%vdt2%vdt1%var_h + 3.0e0

end subroutine sub1

!! Test 2 Sub
subroutine sub2(dtk) bind(c)
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
      type(dt1) :: vdt1
      real(C_DOUBLE) var_a
      logical(C_BOOL) var_b
      real(8) var_c
      character(C_CHAR) var_d
      real(C_LONG_DOUBLE) var_e
      real(4) var_f
      logical(C_BOOL) var_g
      real(C_FLOAT) var_h
   end type

   type, bind(c) :: dt3
      real(C_FLOAT) var_a
      logical(C_BOOL) var_b
      real(C_LONG_DOUBLE) var_c
      real(8) var_d
      real(4) var_e
      logical(C_BOOL) var_f
      real(C_DOUBLE) var_g
      character(C_CHAR) var_h
      type(dt2) :: vdt2
   end type

   type(dt3), value :: dtk

   if ( dtk%var_a /= 2.0e0 .or. dtk%vdt2%var_a /= 2.0d0 .or. & 
                          (dtk%vdt2%vdt1%var_a .neqv. .true.) ) error stop 84
   if ( (dtk%var_b .neqv. .true.) .or. (dtk%vdt2%var_b .neqv. .true.) .or. & 
                          dtk%vdt2%vdt1%var_b /= 4.0d0 ) error stop 86
   if ( dtk%var_c /= 6.0q0 .or. dtk%vdt2%var_c /= 6.0d0 .or. &
                          (dtk%vdt2%vdt1%var_c /= 'A') ) error stop 88
   if ( dtk%var_d /= 8.0d0 .or. (dtk%vdt2%var_d /= 'A') .or. &
                          dtk%vdt2%vdt1%var_d /= 8.0e0 ) error stop 90
   if ( dtk%var_e /= 10.0e0 .or. dtk%vdt2%var_e /= 10.0q0 .or. &
                           dtk%vdt2%vdt1%var_e /= 10.0q0 ) error stop 92
   if ( (dtk%var_f .neqv. .true.) .or. dtk%vdt2%var_f /= 12.0e0 .or. &
                           dtk%vdt2%vdt1%var_f /= 12.0d0 ) error stop 94
   if ( dtk%var_g /= 14.0d0 .or. (dtk%vdt2%var_g .neqv. .true.) .or. &
                           dtk%vdt2%vdt1%var_h /= 16.0e0 ) error stop 96
   if ( (dtk%var_h /= 'A') .or. dtk%vdt2%var_h /= 16.0e0 .or. &
                           dtk%vdt2%vdt1%var_h /= 16.0e0 ) error stop 98

   dtk%var_a = dtk%var_a + 1.0e0
   dtk%var_b = .not. dtk%var_b
   dtk%var_c = dtk%var_c + 1.0q0
   dtk%var_d = dtk%var_d + 1.0d0
   dtk%var_e = dtk%var_e + 1.0e0
   dtk%var_f = .not. dtk%var_f
   dtk%var_g = dtk%var_g + 1.0d0
   dtk%var_h = 'B'

   dtk%vdt2%var_a = dtk%vdt2%var_a + 2.0d0
   dtk%vdt2%var_b = .not. dtk%vdt2%var_b
   dtk%vdt2%var_c = dtk%vdt2%var_c + 2.0d0
   dtk%vdt2%var_d = 'B'
   dtk%vdt2%var_e = dtk%vdt2%var_e + 2.0q0
   dtk%vdt2%var_f = dtk%vdt2%var_f + 2.0e0
   dtk%vdt2%var_g = .not. dtk%vdt2%var_g
   dtk%vdt2%var_h = dtk%vdt2%var_h + 2.0e0

   dtk%vdt2%vdt1%var_a = .not. dtk%vdt2%vdt1%var_a
   dtk%vdt2%vdt1%var_b = dtk%vdt2%vdt1%var_b + 3.0d0
   dtk%vdt2%vdt1%var_c = 'B'
   dtk%vdt2%vdt1%var_d = dtk%vdt2%vdt1%var_d + 3.0e0
   dtk%vdt2%vdt1%var_e = dtk%vdt2%vdt1%var_e + 3.0q0
   dtk%vdt2%vdt1%var_f = dtk%vdt2%vdt1%var_f + 3.0d0
   dtk%vdt2%vdt1%var_g = .not. dtk%vdt2%vdt1%var_g
   dtk%vdt2%vdt1%var_h = dtk%vdt2%vdt1%var_h + 3.0e0

end subroutine sub2

!! Test 3 Sub
subroutine sub3(dtu,dty) bind(c)
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
      type(dt1) :: vdt1
      real(C_DOUBLE) var_a
      logical(C_BOOL) var_b
      real(8) var_c
      character(C_CHAR) var_d
      real(C_LONG_DOUBLE) var_e
      real(4) var_f
      logical(C_BOOL) var_g
      real(C_FLOAT) var_h
   end type

   type, bind(c) :: dt3
      real(C_FLOAT) var_a
      logical(C_BOOL) var_b
      real(C_LONG_DOUBLE) var_c
      real(8) var_d
      real(4) var_e
      logical(C_BOOL) var_f
      real(C_DOUBLE) var_g
      character(C_CHAR) var_h
      type(dt2) :: vdt2
   end type

   type(dt3), intent(in) :: dtu
   type(dt3), intent(out) :: dty

   if ( dtu%var_a /= 4.0e0 .or. dtu%vdt2%var_a /= 4.0d0 .or. &
                            (dtu%vdt2%vdt1%var_a .neqv. .true.) ) error stop 100

   if ( (dtu%var_b .neqv. .true.) .or. (dtu%vdt2%var_b .neqv. .true.) .or. &
                            dtu%vdt2%vdt1%var_b /= 8.0d0 ) error stop 102

   if ( dtu%var_c /= 12.0q0 .or. dtu%vdt2%var_c /= 12.0d0 .or. &
                             (dtu%vdt2%vdt1%var_c /= 'A') ) error stop 104

   if ( dtu%var_d /= 16.0d0 .or. (dtu%vdt2%var_d /= 'A') .or. &
                             dtu%vdt2%vdt1%var_d /= 16.0e0 ) error stop 106

   if ( dtu%var_e /= 20.0e0 .or. dtu%vdt2%var_e /= 20.0q0 .or. &
                             dtu%vdt2%vdt1%var_e /= 20.0q0 ) error stop 108

   if ( (dtu%var_f .neqv. .true.) .or. dtu%vdt2%var_f /= 24.0e0 .or. &
                             dtu%vdt2%vdt1%var_f /= 24.0d0 ) error stop 110

   if ( dtu%var_g /= 28.0d0 .or. (dtu%vdt2%var_g .neqv. .true.) .or. &
                             (dtu%vdt2%vdt1%var_g .neqv. .true.) ) error stop 112

   if ( (dtu%var_h /= 'A') .or. dtu%vdt2%var_h /= 32.0e0 .or. &
                             dtu%vdt2%vdt1%var_h /= 32.0e0 ) error stop 114

   dty%var_a = dtu%var_a + 1.0e0
   dty%var_b = .not. dtu%var_b
   dty%var_c = dtu%var_c + 1.0q0
   dty%var_d = dtu%var_d + 1.0d0
   dty%var_e = dtu%var_e + 1.0e0
   dty%var_f = .not. dtu%var_f
   dty%var_g = dtu%var_g + 1.0d0
   dty%var_h = 'B'

   dty%vdt2%var_a = dtu%vdt2%var_a + 2.0d0
   dty%vdt2%var_b = .not. dtu%vdt2%var_b
   dty%vdt2%var_c = dtu%vdt2%var_c + 2.0d0
   dty%vdt2%var_d = 'B'
   dty%vdt2%var_e = dtu%vdt2%var_e + 2.0q0
   dty%vdt2%var_f = dtu%vdt2%var_f + 2.0e0
   dty%vdt2%var_g = .not. dtu%vdt2%var_g
   dty%vdt2%var_h = dtu%vdt2%var_h + 2.0e0

   dty%vdt2%vdt1%var_a = .not. dtu%vdt2%vdt1%var_a
   dty%vdt2%vdt1%var_b = dtu%vdt2%vdt1%var_b + 3.0d0
   dty%vdt2%vdt1%var_c = 'B'
   dty%vdt2%vdt1%var_d = dtu%vdt2%vdt1%var_d + 3.0e0
   dty%vdt2%vdt1%var_e = dtu%vdt2%vdt1%var_e + 3.0q0
   dty%vdt2%vdt1%var_f = dtu%vdt2%vdt1%var_f + 3.0d0
   dty%vdt2%vdt1%var_g = .not. dtu%vdt2%vdt1%var_g
   dty%vdt2%vdt1%var_h = dtu%vdt2%vdt1%var_h + 3.0e0

end subroutine sub3
