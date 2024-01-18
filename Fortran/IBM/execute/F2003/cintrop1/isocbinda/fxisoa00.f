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
!*  KEYWORD(S)                 : C_INT, C_SHORT
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_INT and C_SHORT
!*	- using external FORTRAN functions
!*	- passing scalar arguments by REFERENCE and by VALUE
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

integer(C_INT) function fnt1(a,b)
   use ISO_C_BINDING

   integer(C_INT) :: a
   integer(C_SHORT) :: b

   if ( a /= 5 ) error stop 20
   if ( b /= 10 ) error stop 22

   a = a + 5
   b = b + 10

   fnt1 = a**2+b
end function fnt1

integer(C_INT) function fnt2(a,b)
   use ISO_C_BINDING

   integer(C_INT), value :: a
   integer(C_SHORT), value :: b

   if ( a /= 5 ) error stop 24
   if ( b /= 10 ) error stop 26

   a = a + 5
   b = b + 10

   fnt2 = a**2+b
end function fnt2

integer(C_INT) function fnt3(a,b)
   use ISO_C_BINDING

   integer(C_INT), intent(in) :: a
   integer(C_SHORT), intent(in) :: b

   if ( a /= 5 ) error stop 28
   if ( b /= 10 ) error stop 30

   fnt3 = a**2+b
end function fnt3

integer(C_INT) function fnt4(a,b)
   use ISO_C_BINDING

   integer(C_INT), intent(in), value :: a
   integer(C_SHORT), intent(in), value :: b

   if ( a /= 5 ) error stop 32
   if ( b /= 10 ) error stop 34

   fnt4 = a**2+b
end function fnt4

integer(C_INT) function fnt5(a,b)
   use ISO_C_BINDING

   integer(C_INT), intent(in) :: a
   integer(C_SHORT), intent(in) :: b

   if ( a /= 5 ) error stop 36
   if ( b /= 10 ) error stop 38

   fnt5 = a**2+b
end function fnt5

integer(C_INT) function fnt6(a,b)
   use ISO_C_BINDING

   integer(C_INT), intent(in), value :: a
   integer(C_SHORT), intent(in), value :: b

   if ( a /= 5 ) error stop 40
   if ( b /= 10 ) error stop 42

   fnt6 = a**2+b
end function fnt6
