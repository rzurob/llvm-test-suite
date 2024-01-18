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
!*  KEYWORD(S)                 : C_FLOAT_COMPLEX, C_DOUBLE_COMPLEX
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_FLOAT_COMPLEX and C_DOUBLE_COMPLEX
!*	- using external FORTRAN functions
!*	- passing scalar arguments by REFERENCE and by VALUE
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

complex(C_FLOAT_COMPLEX) function fnt1(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX) :: a
   complex(C_DOUBLE_COMPLEX) :: b

   if ( a /= (5.0e0,5.0e0) ) error stop 20
   if ( b /= (10.0d0,10.0d0) ) error stop 22

   a = a + (5.0e0,5.0e0)
   b = b + (10.0d0,10.0d0)

   fnt1 = 0
end function fnt1

complex(C_FLOAT_COMPLEX) function fnt2(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), value :: a
   complex(C_DOUBLE_COMPLEX), value :: b

   if ( a /= (5.0e0,5.0e0) ) error stop 24
   if ( b /= (10.0d0,10.0d0) ) error stop 26

   a = a + (5.0e0,5.0e0)
   b = b + (10.0d0,10.0d0)

   fnt2 = 0
end function fnt2

complex(C_FLOAT_COMPLEX) function fnt3(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(in) :: a
   complex(C_DOUBLE_COMPLEX), intent(in) :: b

   if ( a /= (5.0e0,5.0e0) ) error stop 28
   if ( b /= (10.0d0,10.0d0) ) error stop 30

   fnt3 = 0
end function fnt3

complex(C_FLOAT_COMPLEX) function fnt4(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(in), value :: a
   complex(C_DOUBLE_COMPLEX), intent(in), value :: b

   if ( a /= (5.0e0,5.0e0) ) error stop 32
   if ( b /= (10.0d0,10.0d0) ) error stop 34

   fnt4 = 0
end function fnt4

complex(C_FLOAT_COMPLEX) function fnt5(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(in) :: a
   complex(C_DOUBLE_COMPLEX), intent(in) :: b

   if ( a /= (5.0e0,5.0e0) ) error stop 36
   if ( b /= (10.0d0,10.0d0) ) error stop 38

   fnt5 = 0
end function fnt5

complex(C_FLOAT_COMPLEX) function fnt6(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(in), value :: a
   complex(C_DOUBLE_COMPLEX), intent(in), value :: b

   if ( a /= (5.0e0,5.0e0) ) error stop 40
   if ( b /= (10.0d0,10.0d0) ) error stop 42

   fnt6 = 0
end function fnt6
