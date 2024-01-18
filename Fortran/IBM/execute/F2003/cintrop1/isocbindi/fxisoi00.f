!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoi00.presh fxisoi00 cxisoi00
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
!*  KEYWORD(S)                 : C_INT_FAST16_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_INT_FAST16_T
!*	- using external FORTRAN functions
!*	- passing scalar arguments by REFERENCE and by VALUE
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

integer(C_INT_FAST16_T) function fnt1(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T) :: a

   if ( a /= 5 ) error stop 20

   a = a + 5

   fnt1 = a**2
end function fnt1

integer(C_INT_FAST16_T) function fnt2(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), value :: a

   if ( a /= 5 ) error stop 22

   a = a + 5

   fnt2 = a**2
end function fnt2

integer(C_INT_FAST16_T) function fnt3(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in) :: a

   if ( a /= 5 ) error stop 24

   fnt3 = a**2
end function fnt3

integer(C_INT_FAST16_T) function fnt4(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in), value :: a

   if ( a /= 5 ) error stop 26

   fnt4 = a**2
end function fnt4

integer(C_INT_FAST16_T) function fnt5(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in) :: a

   if ( a /= 5 ) error stop 28

   fnt5 = a**2
end function fnt5

integer(C_INT_FAST16_T) function fnt6(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in), value :: a

   if ( a /= 5 ) error stop 30

   fnt6 = a**2
end function fnt6
