!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 4/23/2002
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
!*	- using external FORTRAN subroutines
!*	- passing scalar arguments by REFERENCE and by VALUE
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine sub1(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T) :: a

   if ( a /= 5 ) error stop 20

   a = a + 5

end subroutine sub1

subroutine sub2(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), value :: a

   if ( a /= 5 ) error stop 22

   a = a + 5

end subroutine sub2

subroutine sub3(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in) :: a

   if ( a /= 5 ) error stop 24

end subroutine sub3

subroutine sub4(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in), value :: a

   if ( a /= 5 ) error stop 26

end subroutine sub4

subroutine sub5(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in) :: a

   if ( a /= 5 ) error stop 28

end subroutine sub5

subroutine sub6(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in), value :: a

   if ( a /= 5 ) error stop 30

end subroutine sub6