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
!*  KEYWORD(S)                 : 16
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing 16
!*	- using external FORTRAN subroutines
!*	- passing scalar arguments by REFERENCE and by VALUE
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine sub1(a)
   use ISO_C_BINDING

   real(16) :: a

   if ( a /= 5.0q0 ) error stop 20

   a = a + 5.0q0

end subroutine sub1

subroutine sub2(a)
   use ISO_C_BINDING

   real(16), value :: a

   if ( a /= 5.0q0 ) error stop 22

   a = a + 5.0q0

end subroutine sub2

subroutine sub3(a)
   use ISO_C_BINDING

   real(16), intent(in) :: a

   if ( a /= 5.0q0 ) error stop 24

end subroutine sub3

subroutine sub4(a)
   use ISO_C_BINDING

   real(16), intent(in), value :: a

   if ( a /= 5.0q0 ) error stop 26

end subroutine sub4

subroutine sub5(a)
   use ISO_C_BINDING

   real(16), intent(in) :: a

   if ( a /= 5.0q0 ) error stop 28

end subroutine sub5

subroutine sub6(a)
   use ISO_C_BINDING

   real(16), intent(in), value :: a

   if ( a /= 5.0q0 ) error stop 30

end subroutine sub6
