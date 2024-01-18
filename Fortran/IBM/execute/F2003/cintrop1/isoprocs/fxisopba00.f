!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisopba00 cxisopba00
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
!*      - testing scalar (arrays not allowed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisopba00
   use ISO_C_BINDING, ONLY : C_PTR, C_LOC, C_ASSOCIATED, C_F_POINTER

   interface
      subroutine sub1(x)
         use ISO_C_BINDING, ONLY : C_PTR
         type(C_PTR) :: x
      end subroutine sub1
   end interface

   byte, target :: a
   byte, allocatable, target :: al
   byte, pointer :: pa
   type(C_PTR) :: cp
   
!! Test 1

   a = ichar('A')
   pa => a

   cp = C_LOC(a)
   if ( .not. C_ASSOCIATED(cp) ) error stop 20
   if ( .not. C_ASSOCIATED(cp,C_LOC(a)) ) error stop 22

   call sub1(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 24
   if ( C_ASSOCIATED(cp,C_LOC(a)) ) error stop 26

   call C_F_POINTER(cp,pa)
   if ( ASSOCIATED(pa,a) ) error stop 28
   if ( pa /= ichar('C') ) error stop 30

   pa => a

   cp = C_LOC(pa)
   if ( .not. C_ASSOCIATED(cp) ) error stop 32
   if ( .not. C_ASSOCIATED(cp,C_LOC(pa)) ) error stop 34

   call sub1(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 36
   if ( C_ASSOCIATED(cp,C_LOC(pa)) ) error stop 38

   call C_F_POINTER(cp,pa)
   if ( ASSOCIATED(pa,a) ) error stop 40
   if ( pa /= ichar('C') ) error stop 42

!! Test 2

   allocate(al)

   al = ichar('A')
   pa => al

   cp = C_LOC(al)
   if ( .not. C_ASSOCIATED(cp) ) error stop 68
   if ( .not. C_ASSOCIATED(cp,C_LOC(al)) ) error stop 70

   call sub1(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 72
   if ( C_ASSOCIATED(cp,C_LOC(al)) ) error stop 74

   call C_F_POINTER(cp,pa)
   if ( ASSOCIATED(pa,al) ) error stop 76
   if ( pa /= ichar('C') ) error stop 78

   pa => al

   cp = C_LOC(pa)
   if ( .not. C_ASSOCIATED(cp) ) error stop 80
   if ( .not. C_ASSOCIATED(cp,C_LOC(pa)) ) error stop 82

   call sub1(cp)
   if ( .not. C_ASSOCIATED(cp) ) error stop 84
   if ( C_ASSOCIATED(cp,C_LOC(pa)) ) error stop 86

   call C_F_POINTER(cp,pa)
   if ( ASSOCIATED(pa,a) ) error stop 88
   if ( pa /= ichar('C') ) error stop 90

   deallocate(al)

end program fxisopba00
