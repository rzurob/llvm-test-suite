!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisopbb01 cxisopbb01
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
!*  TEST CASE TITLE            : Support for ISO_C_BINDING module
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquide
!*  DATE                       : 4/23/2002
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*
!*  PRIMARY FUNCTIONS TESTED   : ISO_C_BINDING module
!*  SECONDARY FUNCTIONS TESTED : see below 
!*
!*  DRIVER STANZA              : 
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : byte
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*
!*	- testing non-interoperable kind type parameter byte
!*      - testing procedures C_LOC, C_ASSOCIATED and C_F_POINTER
!*      - testing scalar as fields in derived-types
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisopbb01
   use ISO_C_BINDING, ONLY : C_PTR, C_LOC, C_ASSOCIATED, C_F_POINTER

   interface
      subroutine sub1(x)
         use ISO_C_BINDING, ONLY : C_PTR
         type(C_PTR) :: x
      end subroutine sub1
   end interface

   type :: dts0
      byte :: a
   end type

   type :: dts1
      sequence
      byte :: a
   end type

   type(dts0), target :: dta
   type(dts0), pointer :: pa

   type(dts0), allocatable, target :: dtal

   type(C_PTR) :: cp
   
!! Test 1

   dta%a = 'A'
   pa => dta

   cp = C_LOC(dta)
   if ( .not. C_ASSOCIATED(cp) ) error stop 20
   if ( .not. C_ASSOCIATED(cp,C_LOC(dta)) ) error stop 22

   call sub1(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 24
   if ( C_ASSOCIATED(cp,C_LOC(dta)) ) error stop 26

   call C_F_POINTER(cp,pa)
   if ( ASSOCIATED(pa,dta) ) error stop 28
   if ( pa%a /= 'C' ) error stop 30

!! Test 2

   allocate(dtal)

   dtal%a = 'A'
   pa => dtal

   cp = C_LOC(dtal)
   if ( .not. C_ASSOCIATED(cp) ) error stop 56
   if ( .not. C_ASSOCIATED(cp,C_LOC(dtal)) ) error stop 58

   call sub1(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 60
   if ( C_ASSOCIATED(cp,C_LOC(dtal)) ) error stop 62

   call C_F_POINTER(cp,pa)
   if ( ASSOCIATED(pa,dtal) ) error stop 64
   if ( pa%a /= 'C' ) error stop 66

end program fxisopbb01
