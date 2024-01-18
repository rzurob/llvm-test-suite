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
!*  KEYWORD(S)                 : C_FLOAT, C_DOUBLE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_FLOAT and C_DOUBLE
!*	- using external FORTRAN functions
!*	- passing scalar arguments by REFERENCE and by VALUE
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

real(C_FLOAT) function fnt1(a,b)
   use ISO_C_BINDING

   real(C_FLOAT) :: a
   real(C_DOUBLE) :: b

   if ( a /= 5.0e0 ) error stop 20
   if ( b /= 10.0d0 ) error stop 22

   a = a + 5.0e0
   b = b + 10.0d0

   fnt1 = 0
end function fnt1

real(C_FLOAT) function fnt2(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), value :: a
   real(C_DOUBLE), value :: b

   if ( a /= 5.0e0 ) error stop 24
   if ( b /= 10.0d0 ) error stop 26

   a = a + 5.0e0
   b = b + 10.0d0

   fnt2 = 0
end function fnt2

real(C_FLOAT) function fnt3(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), intent(in) :: a
   real(C_DOUBLE), intent(in) :: b

   if ( a /= 5.0e0 ) error stop 28
   if ( b /= 10.0d0 ) error stop 30

   fnt3 = 0
end function fnt3

real(C_FLOAT) function fnt4(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), intent(in), value :: a
   real(C_DOUBLE), intent(in), value :: b

   if ( a /= 5.0e0 ) error stop 32
   if ( b /= 10.0d0 ) error stop 34

   fnt4 = 0
end function fnt4

real(C_FLOAT) function fnt5(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), intent(in) :: a
   real(C_DOUBLE), intent(in) :: b

   if ( a /= 5.0e0 ) error stop 36
   if ( b /= 10.0d0 ) error stop 38

   fnt5 = 0
end function fnt5

real(C_FLOAT) function fnt6(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), intent(in), value :: a
   real(C_DOUBLE), intent(in), value :: b

   if ( a /= 5.0e0 ) error stop 40
   if ( b /= 10.0d0 ) error stop 42

   fnt6 = 0
end function fnt6
