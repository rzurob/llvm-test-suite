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
!*  KEYWORD(S)                 : C_INT8_T, C_INT16_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_INT8_T and C_INT16_T
!*      - FORTRAN code only
!*      - passing scalar arguments by REFERENCE and by VALUE
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisoc24
   use ISO_C_BINDING

   interface
      integer(C_INT8_T) function fnt1(a,b)
         use ISO_C_BINDING
         integer(C_INT8_T) :: a
         integer(C_INT16_T) :: b
      end function fnt1
      integer(C_INT8_T) function fnt2(a,b)
         use ISO_C_BINDING
         integer(C_INT8_T), value :: a
         integer(C_INT16_T), value :: b
      end function fnt2
      integer(C_INT8_T) function fnt3(a,b)
         use ISO_C_BINDING
         integer(C_INT8_T), intent(in) :: a
         integer(C_INT16_T), intent(in) :: b
      end function fnt3
      integer(C_INT8_T) function fnt4(a,b)
         use ISO_C_BINDING
         integer(C_INT8_T), intent(in), value :: a
         integer(C_INT16_T), intent(in), value :: b
      end function fnt4
   end interface

   integer(C_INT8_T) :: a, ret
   integer(C_INT16_T) :: b

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

end program fxisoc24

integer(C_INT8_T) function fnt1(a,b)
   use ISO_C_BINDING

   integer(C_INT8_T) :: a
   integer(C_INT16_T) :: b

   if ( a /= 5 ) error stop 36
   if ( b /= 10 ) error stop 38

   a = a + 5
   b = b + 10

   fnt1 = a**2+b
end function fnt1

integer(C_INT8_T) function fnt2(a,b)
   use ISO_C_BINDING

   integer(C_INT8_T), value :: a
   integer(C_INT16_T), value :: b

   if ( a /= 5 ) error stop 40
   if ( b /= 10 ) error stop 42

   a = a + 5
   b = b + 10

   fnt2 = a**2+b
end function fnt2

integer(C_INT8_T) function fnt3(a,b)
   use ISO_C_BINDING

   integer(C_INT8_T), intent(in) :: a
   integer(C_INT16_T), intent(in) :: b

   if ( a /= 5 ) error stop 44
   if ( b /= 10 ) error stop 46

   fnt3 = a**2+b
end function fnt3

integer(C_INT8_T) function fnt4(a,b)
   use ISO_C_BINDING

   integer(C_INT8_T), intent(in), value :: a
   integer(C_INT16_T), intent(in), value :: b

   if ( a /= 5 ) error stop 48
   if ( b /= 10 ) error stop 50

   fnt4 = a**2+b
end function fnt4
