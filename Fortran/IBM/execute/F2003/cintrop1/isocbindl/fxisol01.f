!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisol01 cxisol01
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
!*  KEYWORD(S)                 : C_FLOAT, C_DOUBLE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*
!*	- testing C_FLOAT and C_DOUBLE
!*	- using external FORTRAN subroutines
!*	- passing scalar arguments by REFERENCE and by VALUE
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine sub1(a,b)
   use ISO_C_BINDING

   real(C_FLOAT) :: a
   real(C_DOUBLE) :: b
   
   if ( a /= 5.0e0 ) error stop 20
   if ( b /= 10.0d0 ) error stop 22

   a = a + 5.0e0
   b = b + 10.0d0

end subroutine sub1

subroutine sub2(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), value :: a
   real(C_DOUBLE), value :: b
   
   if ( a /= 5.0e0 ) error stop 24
   if ( b /= 10.0d0 ) error stop 26

   a = a + 5.0e0
   b = b + 10.0d0

end subroutine sub2

subroutine sub3(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), intent(in) :: a
   real(C_DOUBLE), intent(in) :: b
   
   if ( a /= 5.0e0 ) error stop 28
   if ( b /= 10.0d0 ) error stop 30

end subroutine sub3

subroutine sub4(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), intent(in), value :: a
   real(C_DOUBLE), intent(in), value :: b
   
   if ( a /= 5.0e0 ) error stop 32
   if ( b /= 10.0d0 ) error stop 34

end subroutine sub4

subroutine sub5(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), intent(in) :: a
   real(C_DOUBLE), intent(in) :: b
   
   if ( a /= 5.0e0 ) error stop 36
   if ( b /= 10.0d0 ) error stop 38

end subroutine sub5

subroutine sub6(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), intent(in), value :: a
   real(C_DOUBLE), intent(in), value :: b
   
   if ( a /= 5.0e0 ) error stop 40
   if ( b /= 10.0d0 ) error stop 42

end subroutine sub6
