!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtye00.presh fxdtyla08b cxdtyla08b
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
      logical(C_BOOL) :: var_a = .true.
      real(C_DOUBLE) :: var_b = 4.0d0
      character(C_CHAR) :: var_c = 'A'
      real(C_FLOAT) :: var_d = 8.0e0
      real(C_LONG_DOUBLE) :: var_e = 10.0q0
      real(8) :: var_f = 12.0d0
      logical(C_BOOL) :: var_g = .true.
      real(4) :: var_h = 16.0e0
   end type

   type, bind(c) :: dt2
      real(C_FLOAT) :: var_a = 2.0e0
      logical(C_BOOL) :: var_b = .true.
      real(C_LONG_DOUBLE) :: var_c = 6.0q0
      real(8) :: var_d = 8.0d0
      real(4) :: var_e = 10.0e0
      logical(C_BOOL) :: var_f = .true.
      real(C_DOUBLE) :: var_g = 14.0d0
      character(C_CHAR) :: var_h = 'A'
      type(dt1) :: vdt1
   end type

end module mxdtyla08

program fxdtyla08b
   use mxdtyla08
   interface
      subroutine sub1(dt) bind(c)
         import dt2
         type(dt2) :: dt
      end subroutine sub1
   end interface

   type(dt2) :: dta

!! Test 1

   call sub1(dta)

   if ( dta%var_a /= 3.0e0 .or. (dta%vdt1%var_a .neqv. .false.) ) error stop 20
   if ( (dta%var_b .neqv. .false.) .or. dta%vdt1%var_b /= 6 ) error stop 22
   if ( dta%var_c /= 7.0q0 .or. (dta%vdt1%var_c /= 'B') ) error stop 24
   if ( dta%var_d /= 9.0d0 .or. dta%vdt1%var_d /= 10 ) error stop 26
   if ( dta%var_e /= 11.0e0 .or. dta%vdt1%var_e /= 12 ) error stop 28
   if ( (dta%var_f .neqv. .false.) .or. dta%vdt1%var_f /= 14 ) error stop 30
   if ( dta%var_g /= 15.0d0 .or. (dta%vdt1%var_g .neqv. .false.) ) error stop 32
   if ( (dta%var_h /= 'B') .or. dta%vdt1%var_h /= 18 ) error stop 34

end program fxdtyla08b
