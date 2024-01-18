!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisor00.presh fxisor00 cxisor00
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
!*  PRIMARY FUNCTIONS TESTED   : ISO_C_BINDING module
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : C_BOOL
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_BOOL
!*	- using external FORTRAN functions
!*	- passing scalar arguments by REFERENCE and by VALUE
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

logical(C_BOOL) function fnt1(a)
   use ISO_C_BINDING

   logical(C_BOOL) :: a

   if ( a .neqv. .true. ) error stop 20

   a = .false.

   fnt1 = .false.
end function fnt1

logical(C_BOOL) function fnt2(a)
   use ISO_C_BINDING

   logical(C_BOOL), value :: a

   if ( a .neqv. .true. ) error stop 22

   a = .false.

   fnt2 = .false.
end function fnt2

logical(C_BOOL) function fnt3(a)
   use ISO_C_BINDING

   logical(C_BOOL), intent(in) :: a

   if ( a .neqv. .true. ) error stop 24

   fnt3 = .false.
end function fnt3

logical(C_BOOL) function fnt4(a)
   use ISO_C_BINDING

   logical(C_BOOL), intent(in), value :: a

   if ( a .neqv. .true. ) error stop 26

   fnt4 = .false.
end function fnt4

logical(C_BOOL) function fnt5(a)
   use ISO_C_BINDING

   logical(C_BOOL), intent(in) :: a

   if ( a .neqv. .true. ) error stop 28

   fnt5 = .false.
end function fnt5

logical(C_BOOL) function fnt6(a)
   use ISO_C_BINDING

   logical(C_BOOL), intent(in), value :: a

   if ( a .neqv. .true. ) error stop 30

   fnt6 = .false.
end function fnt6
