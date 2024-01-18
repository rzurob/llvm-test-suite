!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisom00 cxisom00
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
!*  KEYWORD(S)                 : C_LONG_DOUBLE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_LONG_DOUBLE
!*	- using external FORTRAN functions
!*	- passing scalar arguments by REFERENCE and by VALUE
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

real(C_LONG_DOUBLE) function fnt1(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE) :: a

   if ( a /= 5.0d0 ) error stop 20

   a = a + 5.0d0

   fnt1 = 0
end function fnt1

real(C_LONG_DOUBLE) function fnt2(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), value :: a

   if ( a /= 5.0d0 ) error stop 22

   a = a + 5.0d0

   fnt2 = 0
end function fnt2

real(C_LONG_DOUBLE) function fnt3(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(in) :: a

   if ( a /= 5.0d0 ) error stop 24

   fnt3 = 0
end function fnt3

real(C_LONG_DOUBLE) function fnt4(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(in), value :: a

   if ( a /= 5.0d0 ) error stop 26

   fnt4 = 0
end function fnt4

real(C_LONG_DOUBLE) function fnt5(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(in) :: a

   if ( a /= 5.0d0 ) error stop 28

   fnt5 = 0
end function fnt5

real(C_LONG_DOUBLE) function fnt6(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(in), value :: a

   if ( a /= 5.0d0 ) error stop 30

   fnt6 = 0
end function fnt6
