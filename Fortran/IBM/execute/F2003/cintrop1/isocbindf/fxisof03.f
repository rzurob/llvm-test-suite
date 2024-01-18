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
!*  KEYWORD(S)                 : C_INT_LEAST32_T, C_INT_LEAST64_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_INT_LEAST32_T and C_INT_LEAST64_T
!*      - using C functions with interface to FORTRAN functions
!*      - function interfaces defined in module
!*      - passing scalar arguments by REFERENCE and by VALUE
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob03
   interface
      integer(C_INT_LEAST32_T) function fnt1(a,b)
         use ISO_C_BINDING
         integer(C_INT_LEAST32_T) :: a
         integer(C_INT_LEAST64_T) :: b
      end function fnt1
      integer(C_INT_LEAST32_T) function fnt2(a,b)
         use ISO_C_BINDING
         integer(C_INT_LEAST32_T), value :: a
         integer(C_INT_LEAST64_T), value :: b
      end function fnt2
      integer(C_INT_LEAST32_T) function fnt3(a,b)
         use ISO_C_BINDING
         integer(C_INT_LEAST32_T), intent(in) :: a
         integer(C_INT_LEAST64_T), intent(in) :: b
      end function fnt3
      integer(C_INT_LEAST32_T) function fnt4(a,b)
         use ISO_C_BINDING
         integer(C_INT_LEAST32_T), intent(in), value :: a
         integer(C_INT_LEAST64_T), intent(in), value :: b
      end function fnt4
      integer(C_INT_LEAST32_T) function fnt5(a,b)
         use ISO_C_BINDING
         integer(C_INT_LEAST32_T), intent(in) :: a
         integer(C_INT_LEAST64_T), intent(in) :: b
      end function fnt5
      integer(C_INT_LEAST32_T) function fnt6(a,b)
         use ISO_C_BINDING
         integer(C_INT_LEAST32_T), intent(in), value :: a
         integer(C_INT_LEAST64_T), intent(in), value :: b
      end function fnt6
   end interface
end module mxisob03

program fxisof03
   use ISO_C_BINDING
   use mxisob03

   integer(C_INT_LEAST32_T) :: a, ret
   integer(C_INT_LEAST64_T) :: b

!! Test 1

   a = 5
   b = 10

   ret = fnt1(a,b)

   if ( a /= 10 ) error stop 20
   if ( b /= 20 ) error stop 22

!! Test 2

   a = 5
   b = 10

   ret = fnt2(a,b)

   if ( a /= 5 ) error stop 24
   if ( b /= 10 ) error stop 26

!! Test 3

   a = 5
   b = 10

   ret = fnt3(a,b)

   if ( a /= 5 ) error stop 28
   if ( b /= 10 ) error stop 30

!! Test 4

   a = 5
   b = 10

   ret = fnt4(a,b)

   if ( a /= 5 ) error stop 32
   if ( b /= 10 ) error stop 34

!! Test 5

   a = 5
   b = 10

   ret = fnt5(a,b)

   if ( a /= 5 ) error stop 36
   if ( b /= 10 ) error stop 38

!! Test 6

   a = 5
   b = 10

   ret = fnt6(a,b)

   if ( a /= 5 ) error stop 40
   if ( b /= 10 ) error stop 42

end program fxisof03
