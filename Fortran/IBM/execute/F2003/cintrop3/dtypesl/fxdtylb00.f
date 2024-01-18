!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtyf00.presh fxdtylb00 cxdtyla00
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
!*      - Testing single derived types with VALUE, INTENT attributes
!*      - Testing single derived types with logical, character and real components
!*      - Testing FORTRAN subroutines and C void functions
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxdtylb00
   use ISO_C_BINDING

   type, bind(c) :: dt1
      real(C_FLOAT) var_a
      logical(C_BOOL) var_b
      real(16) var_c
      real(8) var_d
      real(4) var_e
      logical(C_BOOL) var_f
      real(C_DOUBLE) var_g
      character(C_CHAR) var_h
   end type

end module mxdtylb00

module auxmod
   use mxdtylb00

   interface operator(+)
      module procedure adddty
   end interface

!----------------------------------------------------------
contains
   function adddty(dtx,dty)
      type(dt1), intent(in) :: dtx, dty
      type(dt1) :: adddty

      adddty%var_a = dtx%var_a + dty%var_a
      adddty%var_b = dtx%var_b .and. dty%var_b
      adddty%var_c = dtx%var_c + dty%var_c
      adddty%var_d = dtx%var_d + dty%var_d
      adddty%var_e = dtx%var_e + dty%var_e
      adddty%var_f = dtx%var_f .and. dty%var_f
      adddty%var_g = dtx%var_g + dty%var_g
      adddty%var_h = dtx%var_h

   end function adddty
end module auxmod

program fxdtylb00
   use mxdtylb00
   use auxmod
   interface
      subroutine sub1(dt) bind(c)
         import dt1
         type(dt1) :: dt
      end subroutine sub1
      subroutine sub2(dt) bind(c)
         import dt1
         type(dt1), value :: dt
      end subroutine sub2
      subroutine sub3(dtx,dty) bind(c)
         import dt1
         type(dt1), intent(in) :: dtx
         type(dt1), intent(out) :: dty
      end subroutine sub3
   end interface

   type(dt1) :: dt0 = dt1(2.0e0,.true.,6.0q0,8.0d0,10.0e0,.true.,14.0d0,'A')

   type(dt1) :: dta, dtb

!----------------------------------------------------------
!! Test 1

   dta = dt0

   call sub1(dta)

   if ( dta%var_a /= 3.0e0 ) error stop 20
   if ( (dta%var_b .neqv. .false.) ) error stop 22
   if ( dta%var_c /= 7.0q0 ) error stop 24
   if ( dta%var_d /= 9.0d0 ) error stop 26
   if ( dta%var_e /= 11.0e0 ) error stop 28
   if ( (dta%var_f .neqv. .false.) ) error stop 30
   if ( dta%var_g /= 15.0d0 ) error stop 32
   if ( (dta%var_h /= 'B') ) error stop 34

!----------------------------------------------------------
!! Test 2

   dta = dt0

   call sub2(dta)

   if ( dta%var_a /= 2.0e0 ) error stop 36
   if ( (dta%var_b .neqv. .true.) ) error stop 38
   if ( dta%var_c /= 6.0q0 ) error stop 40
   if ( dta%var_d /= 8.0d0 ) error stop 42
   if ( dta%var_e /= 10.0e0 ) error stop 44
   if ( (dta%var_f .neqv. .true.) ) error stop 46
   if ( dta%var_g /= 14.0d0 ) error stop 48
   if ( (dta%var_h /= 'A') ) error stop 50

!----------------------------------------------------------
!! Test 3

   dta = dt0

   call sub3(dta+dta,dtb)

   if ( dtb%var_a /= 5.0e0 ) error stop 52
   if ( (dtb%var_b .neqv. .false.) ) error stop 54
   if ( dtb%var_c /= 13.0q0 ) error stop 56
   if ( dtb%var_d /= 17.0d0 ) error stop 58
   if ( dtb%var_e /= 21.0e0 ) error stop 60
   if ( (dtb%var_f .neqv. .false.) ) error stop 62
   if ( dtb%var_g /= 29.0d0 ) error stop 64
   if ( (dtb%var_h /= 'B') ) error stop 66

!----------------------------------------------------------
end program fxdtylb00
