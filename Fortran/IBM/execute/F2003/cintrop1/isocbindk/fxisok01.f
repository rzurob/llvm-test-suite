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
!*  KEYWORD(S)                 : C_CHAR, C_SIGNED_CHAR
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_CHAR and C_SIGNED_CHAR
!*	- using external FORTRAN subroutines
!*	- passing scalar arguments by REFERENCE and by VALUE
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine sub1(a,b)
   use ISO_C_BINDING

   character(C_CHAR) :: a
   integer(C_SIGNED_CHAR) :: b

   if ( a /= 'A' ) error stop 20
   if ( b /= iachar('B') ) error stop 22

   a = 'C'
   b = iachar('D')

end subroutine sub1

subroutine sub2(a,b)
   use ISO_C_BINDING

   character(C_CHAR), value :: a
   integer(C_SIGNED_CHAR), value :: b

   if ( a /= 'A' ) error stop 24
   if ( b /= iachar('B') ) error stop 26

   a = 'C'
   b = iachar('D')

end subroutine sub2

subroutine sub3(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(in) :: a
   integer(C_SIGNED_CHAR), intent(in) :: b

   if ( a /= 'A' ) error stop 28
   if ( b /= iachar('B') ) error stop 30

end subroutine sub3

subroutine sub4(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(in), value :: a
   integer(C_SIGNED_CHAR), intent(in), value :: b

   if ( a /= 'A' ) error stop 32
   if ( b /= iachar('B') ) error stop 34

end subroutine sub4

subroutine sub5(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(in) :: a
   integer(C_SIGNED_CHAR), intent(in) :: b

   if ( a /= 'A' ) error stop 36
   if ( b /= iachar('B') ) error stop 38

end subroutine sub5

subroutine sub6(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(in), value :: a
   integer(C_SIGNED_CHAR), intent(in), value :: b

   if ( a /= 'A' ) error stop 40
   if ( b /= iachar('B') ) error stop 42

end subroutine sub6
