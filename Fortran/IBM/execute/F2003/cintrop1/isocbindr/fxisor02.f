!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisor00.presh fxisor02 cxisor02
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
!*      - using C functions with interface to FORTRAN functions
!*      - passing scalar arguments by REFERENCE and by VALUE
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisor02
   use ISO_C_BINDING

   interface
      logical(C_BOOL) function fnt1(a)
         use ISO_C_BINDING
         logical(C_BOOL) :: a
      end function fnt1
      logical(C_BOOL) function fnt2(a)
         use ISO_C_BINDING
         logical(C_BOOL), value :: a
      end function fnt2
      logical(C_BOOL) function fnt3(a)
         use ISO_C_BINDING
         logical(C_BOOL), intent(in) :: a
      end function fnt3
      logical(C_BOOL) function fnt4(a)
         use ISO_C_BINDING
         logical(C_BOOL), intent(in), value :: a
      end function fnt4
      logical(C_BOOL) function fnt5(a)
         use ISO_C_BINDING
         logical(C_BOOL), intent(in) :: a
      end function fnt5
      logical(C_BOOL) function fnt6(a)
         use ISO_C_BINDING
         logical(C_BOOL), intent(in), value :: a
      end function fnt6
   end interface

   logical(C_BOOL) :: a, ret

!! Test 1

   a = .true.

   ret = fnt1(a)

   if ( a .eqv. .true. ) error stop 20

!! Test 2

   a = .true.

   ret = fnt2(a)

   if ( a .neqv. .true. ) error stop 22

!! Test 3

   a = .true.

   ret = fnt3(a)

   if ( a .neqv. .true. ) error stop 24

!! Test 4

   a = .true.

   ret = fnt4(a)

   if ( a .neqv. .true. ) error stop 26

!! Test 5

   a = .true.

   ret = fnt5(a)

   if ( a .neqv. .true. ) error stop 28

!! Test 6

   a = .true.

   ret = fnt6(a)

   if ( a .neqv. .true. ) error stop 30

end program fxisor02
