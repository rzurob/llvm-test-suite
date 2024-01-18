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
!*  KEYWORD(S)                 : C_INT8_T, C_INT16_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_INT8_T and C_INT16_T
!*	- using external FORTRAN subroutines
!*	- passing scalar arguments by REFERENCE and by VALUE
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine sub1(a,b)
   use ISO_C_BINDING

   integer(C_INT8_T) :: a
   integer(C_INT16_T) :: b

   if ( a /= 5 ) error stop 20
   if ( b /= 10 ) error stop 22

   a = a + 5
   b = b + 10

end subroutine sub1

subroutine sub2(a,b)
   use ISO_C_BINDING

   integer(C_INT8_T), value :: a
   integer(C_INT16_T), value :: b

   if ( a /= 5 ) error stop 24
   if ( b /= 10 ) error stop 26

   a = a + 5
   b = b + 10

end subroutine sub2

subroutine sub3(a,b)
   use ISO_C_BINDING

   integer(C_INT8_T), intent(in) :: a
   integer(C_INT16_T), intent(in) :: b

   if ( a /= 5 ) error stop 28
   if ( b /= 10 ) error stop 30

end subroutine sub3

subroutine sub4(a,b)
   use ISO_C_BINDING

   integer(C_INT8_T), intent(in), value :: a
   integer(C_INT16_T), intent(in), value :: b

   if ( a /= 5 ) error stop 32
   if ( b /= 10 ) error stop 34

end subroutine sub4

subroutine sub5(a,b)
   use ISO_C_BINDING

   integer(C_INT8_T), intent(in) :: a
   integer(C_INT16_T), intent(in) :: b

   if ( a /= 5 ) error stop 36
   if ( b /= 10 ) error stop 38

end subroutine sub5

subroutine sub6(a,b)
   use ISO_C_BINDING

   integer(C_INT8_T), intent(in), value :: a
   integer(C_INT16_T), intent(in), value :: b

   if ( a /= 5 ) error stop 40
   if ( b /= 10 ) error stop 42

end subroutine sub6
