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
!*	- using external FORTRAN functions
!*	- passing scalar arguments by REFERENCE and by VALUE
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

real(16) function fnt1(a)
   use ISO_C_BINDING

   real(16) :: a

   if ( a /= 5.0q0 ) error stop 20

   a = a + 5.0q0

   fnt1 = 0
end function fnt1

real(16) function fnt2(a)
   use ISO_C_BINDING

   real(16), value :: a

   if ( a /= 5.0q0 ) error stop 22

   a = a + 5.0q0

   fnt2 = 0
end function fnt2

real(16) function fnt3(a)
   use ISO_C_BINDING

   real(16), intent(in) :: a

   if ( a /= 5.0q0 ) error stop 24

   fnt3 = 0
end function fnt3

real(16) function fnt4(a)
   use ISO_C_BINDING

   real(16), intent(in), value :: a

   if ( a /= 5.0q0 ) error stop 26

   fnt4 = 0
end function fnt4

real(16) function fnt5(a)
   use ISO_C_BINDING

   real(16), intent(in) :: a

   if ( a /= 5.0q0 ) error stop 28

   fnt5 = 0
end function fnt5

real(16) function fnt6(a)
   use ISO_C_BINDING

   real(16), intent(in), value :: a

   if ( a /= 5.0q0 ) error stop 30

   fnt6 = 0
end function fnt6