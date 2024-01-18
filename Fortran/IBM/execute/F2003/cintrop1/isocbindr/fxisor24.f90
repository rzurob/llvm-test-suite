!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa01.presh fxisor24
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
!*      - FORTRAN code only
!*      - passing scalar arguments by REFERENCE and by VALUE
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisor24
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

end program fxisor24

logical(C_BOOL) function fnt1(a)
   use ISO_C_BINDING

   logical(C_BOOL) :: a
   
   if ( a .neqv. .true. ) error stop 28

   a = .false.

   fnt1 = .false.
end function fnt1

logical(C_BOOL) function fnt2(a)
   use ISO_C_BINDING

   logical(C_BOOL), value :: a
   
   if ( a .neqv. .true. ) error stop 30

   a = .false.

   fnt2 = .false.
end function fnt2

logical(C_BOOL) function fnt3(a)
   use ISO_C_BINDING

   logical(C_BOOL), intent(in) :: a
   
   if ( a .neqv. .true. ) error stop 32

   fnt3 = .false.
end function fnt3

logical(C_BOOL) function fnt4(a)
   use ISO_C_BINDING

   logical(C_BOOL), intent(in), value :: a
   
   if ( a .neqv. .true. ) error stop 34

   fnt4 = .false.
end function fnt4
