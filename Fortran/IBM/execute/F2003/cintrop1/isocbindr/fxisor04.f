!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisor00.presh fxisor04 cxisor04
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
!*  KEYWORD(S)                 : C_BOOL
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_BOOL
!*      - using C functions with interface to FORTRAN subroutines
!*      - passing scalar arguments by REFERENCE and by VALUE
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisor04
   use ISO_C_BINDING

   interface
      subroutine sub1(a)
         use ISO_C_BINDING
         logical(C_BOOL) :: a
      end subroutine sub1
      subroutine sub2(a)
         use ISO_C_BINDING
         logical(C_BOOL), value :: a
      end subroutine sub2
      subroutine sub3(a)
         use ISO_C_BINDING
         logical(C_BOOL), intent(in) :: a
      end subroutine sub3
      subroutine sub4(a)
         use ISO_C_BINDING
         logical(C_BOOL), intent(in), value :: a
      end subroutine sub4
      subroutine sub5(a)
         use ISO_C_BINDING
         logical(C_BOOL), intent(in) :: a
      end subroutine sub5
      subroutine sub6(a)
         use ISO_C_BINDING
         logical(C_BOOL), intent(in), value :: a
      end subroutine sub6
   end interface

   logical(C_BOOL) :: a, ret

!! Test 1

   a = .true.

   call sub1(a)

   if ( a .eqv. .true. ) error stop 20

!! Test 2

   a = .true.

   call sub2(a)

   if ( a .neqv. .true. ) error stop 22

!! Test 3

   a = .true.

   call sub3(a)

   if ( a .neqv. .true. ) error stop 24

!! Test 4

   a = .true.

   call sub4(a)

   if ( a .neqv. .true. ) error stop 26

!! Test 5

   a = .true.

   call sub5(a)

   if ( a .neqv. .true. ) error stop 28

!! Test 6

   a = .true.

   call sub6(a)

   if ( a .neqv. .true. ) error stop 30

end program fxisor04
