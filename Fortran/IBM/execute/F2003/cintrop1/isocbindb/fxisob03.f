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
!*  KEYWORD(S)                 : C_LONG, C_LONG_LONG
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_LONG and C_LONG_LONG
!*      - using C functions with interface to FORTRAN functions
!*      - function interfaces defined in module
!*      - passing scalar arguments by REFERENCE and by VALUE
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob03
   interface
      integer(C_LONG) function fnt1(a,b)
         use ISO_C_BINDING
         integer(C_LONG) :: a
         integer(C_LONG_LONG) :: b
      end function fnt1
      integer(C_LONG) function fnt2(a,b)
         use ISO_C_BINDING
         integer(C_LONG), value :: a
         integer(C_LONG_LONG), value :: b
      end function fnt2
      integer(C_LONG) function fnt3(a,b)
         use ISO_C_BINDING
         integer(C_LONG), intent(in) :: a
         integer(C_LONG_LONG), intent(in) :: b
      end function fnt3
      integer(C_LONG) function fnt4(a,b)
         use ISO_C_BINDING
         integer(C_LONG), intent(in), value :: a
         integer(C_LONG_LONG), intent(in), value :: b
      end function fnt4
      integer(C_LONG) function fnt5(a,b)
         use ISO_C_BINDING
         integer(C_LONG), intent(in) :: a
         integer(C_LONG_LONG), intent(in) :: b
      end function fnt5
      integer(C_LONG) function fnt6(a,b)
         use ISO_C_BINDING
         integer(C_LONG), intent(in), value :: a
         integer(C_LONG_LONG), intent(in), value :: b
      end function fnt6
   end interface
end module mxisob03

program fxisob03
   use ISO_C_BINDING
   use mxisob03

   integer(C_LONG) :: a, ret
   integer(C_LONG_LONG) :: b

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

end program fxisob03