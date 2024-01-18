!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisok00 cxisok00
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
!*  KEYWORD(S)                 : C_CHAR, C_SIGNED_CHAR
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*
!*	- testing C_CHAR and C_SIGNED_CHAR
!*	- using external FORTRAN functions
!*	- passing scalar arguments by REFERENCE and by VALUE
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

integer(C_SIGNED_CHAR) function fnt1(a,b)
   use ISO_C_BINDING

   character(C_CHAR) :: a
   integer(C_SIGNED_CHAR) :: b
   
   if ( a /= 'A' ) error stop 20
   if ( b /= iachar('B') ) error stop 22

   a = 'C'
   b = iachar('D')

   fnt1 = 0
end function fnt1

integer(C_SIGNED_CHAR) function fnt2(a,b)
   use ISO_C_BINDING

   character(C_CHAR), value :: a
   integer(C_SIGNED_CHAR), value :: b
   
   if ( a /= 'A' ) error stop 24
   if ( b /= iachar('B') ) error stop 26

   a = 'C'
   b = iachar('D')

   fnt2 = 0
end function fnt2

integer(C_SIGNED_CHAR) function fnt3(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(in) :: a
   integer(C_SIGNED_CHAR), intent(in) :: b
   
   if ( a /= 'A' ) error stop 28
   if ( b /= iachar('B') ) error stop 30

   fnt3 = 0
end function fnt3

integer(C_SIGNED_CHAR) function fnt4(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(in), value :: a
   integer(C_SIGNED_CHAR), intent(in), value :: b
   
   if ( a /= 'A' ) error stop 32
   if ( b /= iachar('B') ) error stop 34

   fnt4 = 0
end function fnt4

integer(C_SIGNED_CHAR) function fnt5(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(in) :: a
   integer(C_SIGNED_CHAR), intent(in) :: b
   
   if ( a /= 'A' ) error stop 36
   if ( b /= iachar('B') ) error stop 38

   fnt5 = 0
end function fnt5

integer(C_SIGNED_CHAR) function fnt6(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(in), value :: a
   integer(C_SIGNED_CHAR), intent(in), value :: b
   
   if ( a /= 'A' ) error stop 40
   if ( b /= iachar('B') ) error stop 42

   fnt6 = 0
end function fnt6
