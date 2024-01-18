!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrdtyf00.presh fxdtylb02 cxdtyla02
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
!*      - Testing single derived types with logical, character and real components
!*      - Testing FORTRAN subroutines and C void functions
!*      - Main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxdtylb02
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

end module mxdtylb02

!! Test 1 Sub
subroutine sub1(dt) bind(c)
   use mxdtylb02
   type(dt1) :: dt

   if ( dt%var_a /= 2.0e0 ) error stop 20
   if ( (dt%var_b .neqv. .false.) ) error stop 22
   if ( dt%var_c /= 6.0q0 ) error stop 24
   if ( dt%var_d /= 8.0d0 ) error stop 26
   if ( dt%var_e /= 10.0e0 ) error stop 28
   if ( (dt%var_f .neqv. .false.) ) error stop 30
   if ( dt%var_g /= 14.0d0 ) error stop 32
   if ( (dt%var_h /= 'A') ) error stop 34

   dt%var_a = dt%var_a + 1.0e0
   dt%var_b = .not. dt%var_b
   dt%var_c = dt%var_c + 1.0q0
   dt%var_d = dt%var_d + 1.0d0
   dt%var_e = dt%var_e + 1.0e0
   dt%var_f = .not. dt%var_f
   dt%var_g = dt%var_g + 1.0d0
   dt%var_h = 'B'

end subroutine sub1
!----------------------------------------------------------

!! Test 2 Sub
subroutine sub2(dt) bind(c)
   use mxdtylb02
   type(dt1), value :: dt

   if ( dt%var_a /= 2.0e0 ) error stop 36
   if ( (dt%var_b .neqv. .false.) ) error stop 38
   if ( dt%var_c /= 6.0q0 ) error stop 40
   if ( dt%var_d /= 8.0d0 ) error stop 42
   if ( dt%var_e /= 10.0e0 ) error stop 44
   if ( (dt%var_f .neqv. .false.) ) error stop 46
   if ( dt%var_g /= 14.0d0 ) error stop 48
   if ( (dt%var_h /= 'A') ) error stop 50

   dt%var_a = dt%var_a + 1.0e0
   dt%var_b = .not. dt%var_b
   dt%var_c = dt%var_c + 1.0q0
   dt%var_d = dt%var_d + 1.0d0
   dt%var_e = dt%var_e + 1.0e0
   dt%var_f = .not. dt%var_f
   dt%var_g = dt%var_g + 1.0d0
   dt%var_h = 'B'

end subroutine sub2
!----------------------------------------------------------

!! Test 3 Sub
subroutine sub3(dtx,dty) bind(c)
   use mxdtylb02
   type(dt1), intent(in) :: dtx
   type(dt1), intent(out) :: dty

   if ( dtx%var_a /= 4.0e0 ) error stop 52
   if ( (dtx%var_b .neqv. .false.) ) error stop 54
   if ( dtx%var_c /= 12.0q0 ) error stop 56
   if ( dtx%var_d /= 16.0d0 ) error stop 58
   if ( dtx%var_e /= 20.0e0 ) error stop 60
   if ( (dtx%var_f .neqv. .false.) ) error stop 62
   if ( dtx%var_g /= 28.0d0 ) error stop 64
   if ( (dtx%var_h /= 'A') ) error stop 66

   dty%var_a = dtx%var_a + 1.0e0
   dty%var_b = .not. dtx%var_b
   dty%var_c = dtx%var_c + 1.0q0
   dty%var_d = dtx%var_d + 1.0d0
   dty%var_e = dtx%var_e + 1.0e0
   dty%var_f = .not. dtx%var_f
   dty%var_g = dtx%var_g + 1.0d0
   dty%var_h = 'B'
   
end subroutine sub3
!----------------------------------------------------------
