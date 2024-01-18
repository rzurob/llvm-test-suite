!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisoa01 cxisoa01
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
!*  KEYWORD(S)                 : C_INT, C_SHORT
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_INT and C_SHORT
!*	- using external FORTRAN subroutines
!*	- passing scalar arguments by REFERENCE and by VALUE
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine sub1(a,b)
   use ISO_C_BINDING

   integer(C_INT) :: a
   integer(C_SHORT) :: b

   if ( a /= 5 ) error stop 20
   if ( b /= 10 ) error stop 22

   a = a + 5
   b = b + 10

end subroutine sub1

subroutine sub2(a,b)
   use ISO_C_BINDING

   integer(C_INT), value :: a
   integer(C_SHORT), value :: b

   if ( a /= 5 ) error stop 24
   if ( b /= 10 ) error stop 26

   a = a + 5
   b = b + 10

end subroutine sub2

subroutine sub3(a,b)
   use ISO_C_BINDING

   integer(C_INT), intent(in) :: a
   integer(C_SHORT), intent(in) :: b

   if ( a /= 5 ) error stop 28
   if ( b /= 10 ) error stop 30

end subroutine sub3

subroutine sub4(a,b)
   use ISO_C_BINDING

   integer(C_INT), intent(in), value :: a
   integer(C_SHORT), intent(in), value :: b

   if ( a /= 5 ) error stop 32
   if ( b /= 10 ) error stop 34

end subroutine sub4

subroutine sub5(a,b)
   use ISO_C_BINDING

   integer(C_INT), intent(in) :: a
   integer(C_SHORT), intent(in) :: b

   if ( a /= 5 ) error stop 36
   if ( b /= 10 ) error stop 38

end subroutine sub5

subroutine sub6(a,b)
   use ISO_C_BINDING

   integer(C_INT), intent(in), value :: a
   integer(C_SHORT), intent(in), value :: b

   if ( a /= 5 ) error stop 40
   if ( b /= 10 ) error stop 42

end subroutine sub6
