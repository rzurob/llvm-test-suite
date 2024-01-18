!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisom04 cxisom04
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
!*  KEYWORD(S)                 : C_LONG_DOUBLE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_LONG_DOUBLE
!*      - using C functions with interface to FORTRAN subroutines
!*      - passing scalar arguments by REFERENCE and by VALUE
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisom04
   use ISO_C_BINDING

   interface
      subroutine sub1(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE) :: a
      end subroutine sub1
      subroutine sub2(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), value :: a
      end subroutine sub2
      subroutine sub3(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), intent(in) :: a
      end subroutine sub3
      subroutine sub4(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), intent(in), value :: a
      end subroutine sub4
      subroutine sub5(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), intent(in) :: a
      end subroutine sub5
      subroutine sub6(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), intent(in), value :: a
      end subroutine sub6
   end interface

   real(C_LONG_DOUBLE) :: a, ret

!! Test 1

   a = 5.0d0

   call sub1(a)

   if ( a /= 10.0d0 ) error stop 20

!! Test 2

   a = 5.0d0

   call sub2(a)

   if ( a /= 5.0d0 ) error stop 22

!! Test 3

   a = 5.0d0

   call sub3(a)

   if ( a /= 5.0d0 ) error stop 24

!! Test 4

   a = 5.0d0

   call sub4(a)

   if ( a /= 5.0d0 ) error stop 26

!! Test 5

   a = 5.0d0

   call sub5(a)

   if ( a /= 5.0d0 ) error stop 28

!! Test 6

   a = 5.0d0

   call sub6(a)

   if ( a /= 5.0d0 ) error stop 30

end program fxisom04
