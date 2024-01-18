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
!*  KEYWORD(S)                 : C_LONG_DOUBLE_COMPLEX
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_LONG_DOUBLE_COMPLEX
!*      - using C functions with interface to FORTRAN functions
!*      - passing scalar arguments by REFERENCE and by VALUE
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisoo02
   use ISO_C_BINDING

   interface
      complex(C_LONG_DOUBLE_COMPLEX) function fnt1(a)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX) :: a
      end function fnt1
      complex(C_LONG_DOUBLE_COMPLEX) function fnt2(a)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX), value :: a
      end function fnt2
      complex(C_LONG_DOUBLE_COMPLEX) function fnt3(a)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX), intent(in) :: a
      end function fnt3
      complex(C_LONG_DOUBLE_COMPLEX) function fnt4(a)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX), intent(in), value :: a
      end function fnt4
      complex(C_LONG_DOUBLE_COMPLEX) function fnt5(a)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX), intent(in) :: a
      end function fnt5
      complex(C_LONG_DOUBLE_COMPLEX) function fnt6(a)
         use ISO_C_BINDING
         complex(C_LONG_DOUBLE_COMPLEX), intent(in), value :: a
      end function fnt6
   end interface

   complex(C_LONG_DOUBLE_COMPLEX) :: a, ret

!! Test 1

   a = (5.0d0,5.0d0)

   ret = fnt1(a)

   if ( a /= (10.0d0,10.0d0) ) error stop 20

!! Test 2

   a = (5.0d0,5.0d0)

   ret = fnt2(a)

   if ( a /= (5.0d0,5.0d0) ) error stop 22

!! Test 3

   a = (5.0d0,5.0d0)

   ret = fnt3(a)

   if ( a /= (5.0d0,5.0d0) ) error stop 24

!! Test 4

   a = (5.0d0,5.0d0)

   ret = fnt4(a)

   if ( a /= (5.0d0,5.0d0) ) error stop 26

!! Test 5

   a = (5.0d0,5.0d0)

   ret = fnt5(a)

   if ( a /= (5.0d0,5.0d0) ) error stop 28

!! Test 6

   a = (5.0d0,5.0d0)

   ret = fnt6(a)

   if ( a /= (5.0d0,5.0d0) ) error stop 30

end program fxisoo02
