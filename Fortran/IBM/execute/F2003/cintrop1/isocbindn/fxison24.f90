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
!*      - testing C_FLOAT_COMPLEX and C_DOUBLE_COMPLEX
!*      - FORTRAN code only
!*      - passing scalar arguments by REFERENCE and by VALUE
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxison24
   use ISO_C_BINDING

   interface
      complex(C_FLOAT_COMPLEX) function fnt1(a,b)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX) :: a
         complex(C_DOUBLE_COMPLEX) :: b
      end function fnt1
      complex(C_FLOAT_COMPLEX) function fnt2(a,b)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX), value :: a
         complex(C_DOUBLE_COMPLEX), value :: b
      end function fnt2
      complex(C_FLOAT_COMPLEX) function fnt3(a,b)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX), intent(in) :: a
         complex(C_DOUBLE_COMPLEX), intent(in) :: b
      end function fnt3
      complex(C_FLOAT_COMPLEX) function fnt4(a,b)
         use ISO_C_BINDING
         complex(C_FLOAT_COMPLEX), intent(in), value :: a
         complex(C_DOUBLE_COMPLEX), intent(in), value :: b
      end function fnt4
   end interface

   complex(C_FLOAT_COMPLEX) :: a, ret
   complex(C_DOUBLE_COMPLEX) :: b

!! Test 1

   a = (5.0e0,5.0e0)
   b = (10.0d0,10.0d0)

   ret = fnt1(a,b)

   if ( a /= (10.0e0,10.0e0) ) error stop 20
   if ( b /= (20.0d0,20.0d0) ) error stop 22

!! Test 2

   a = (5.0e0,5.0e0)
   b = (10.0d0,10.0d0)

   ret = fnt2(a,b)

   if ( a /= (5.0e0,5.0e0) ) error stop 24
   if ( b /= (10.0d0,10.0d0) ) error stop 26

!! Test 3

   a = (5.0e0,5.0e0)
   b = (10.0d0,10.0d0)

   ret = fnt3(a,b)

   if ( a /= (5.0e0,5.0e0) ) error stop 28
   if ( b /= (10.0d0,10.0d0) ) error stop 30

!! Test 4

   a = (5.0e0,5.0e0)
   b = (10.0d0,10.0d0)

   ret = fnt4(a,b)

   if ( a /= (5.0e0,5.0e0) ) error stop 32
   if ( b /= (10.0d0,10.0d0) ) error stop 34

end program fxison24

complex(C_FLOAT_COMPLEX) function fnt1(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX) :: a
   complex(C_DOUBLE_COMPLEX) :: b

   if ( a /= (5.0e0,5.0e0) ) error stop 36
   if ( b /= (10.0d0,10.0d0) ) error stop 38

   a = a + (5.0e0,5.0e0)
   b = b + (10.0d0,10.0d0)

   fnt1 = 0
end function fnt1

complex(C_FLOAT_COMPLEX) function fnt2(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), value :: a
   complex(C_DOUBLE_COMPLEX), value :: b

   if ( a /= (5.0e0,5.0e0) ) error stop 40
   if ( b /= (10.0d0,10.0d0) ) error stop 42

   a = a + (5.0e0,5.0e0)
   b = b + (10.0d0,10.0d0)

   fnt2 = 0
end function fnt2

complex(C_FLOAT_COMPLEX) function fnt3(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(in) :: a
   complex(C_DOUBLE_COMPLEX), intent(in) :: b

   if ( a /= (5.0e0,5.0e0) ) error stop 44
   if ( b /= (10.0d0,10.0d0) ) error stop 46

   fnt3 = 0
end function fnt3

complex(C_FLOAT_COMPLEX) function fnt4(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(in), value :: a
   complex(C_DOUBLE_COMPLEX), intent(in), value :: b

   if ( a /= (5.0e0,5.0e0) ) error stop 48
   if ( b /= (10.0d0,10.0d0) ) error stop 50

   fnt4 = 0
end function fnt4
