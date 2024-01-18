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
!*  KEYWORD(S)                 : C_SIZE_T, C_INTPTR_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_SIZE_T and C_INTPTR_T
!*	- using external FORTRAN functions
!*	- passing scalar arguments by REFERENCE and by VALUE
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

integer(C_SIZE_T) function fnt1(a,b)
   use ISO_C_BINDING

   integer(C_SIZE_T) :: a
   integer(C_INTPTR_T) :: b

   if ( a /= 5 ) error stop 20
   if ( b /= 10 ) error stop 22

   a = a + 5
   b = b + 10

   fnt1 = a**2+b
end function fnt1

integer(C_SIZE_T) function fnt2(a,b)
   use ISO_C_BINDING

   integer(C_SIZE_T), value :: a
   integer(C_INTPTR_T), value :: b

   if ( a /= 5 ) error stop 24
   if ( b /= 10 ) error stop 26

   a = a + 5
   b = b + 10

   fnt2 = a**2+b
end function fnt2

integer(C_SIZE_T) function fnt3(a,b)
   use ISO_C_BINDING

   integer(C_SIZE_T), intent(in) :: a
   integer(C_INTPTR_T), intent(in) :: b

   if ( a /= 5 ) error stop 28
   if ( b /= 10 ) error stop 30

   fnt3 = a**2+b
end function fnt3

integer(C_SIZE_T) function fnt4(a,b)
   use ISO_C_BINDING

   integer(C_SIZE_T), intent(in), value :: a
   integer(C_INTPTR_T), intent(in), value :: b

   if ( a /= 5 ) error stop 32
   if ( b /= 10 ) error stop 34

   fnt4 = a**2+b
end function fnt4

integer(C_SIZE_T) function fnt5(a,b)
   use ISO_C_BINDING

   integer(C_SIZE_T), intent(in) :: a
   integer(C_INTPTR_T), intent(in) :: b

   if ( a /= 5 ) error stop 36
   if ( b /= 10 ) error stop 38

   fnt5 = a**2+b
end function fnt5

integer(C_SIZE_T) function fnt6(a,b)
   use ISO_C_BINDING

   integer(C_SIZE_T), intent(in), value :: a
   integer(C_INTPTR_T), intent(in), value :: b

   if ( a /= 5 ) error stop 40
   if ( b /= 10 ) error stop 42

   fnt6 = a**2+b
end function fnt6
