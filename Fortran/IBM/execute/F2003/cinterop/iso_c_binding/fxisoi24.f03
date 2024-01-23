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
!*  KEYWORD(S)                 : C_INT_FAST16_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_INT_FAST16_T
!*      - FORTRAN code only
!*      - passing scalar arguments by REFERENCE and by VALUE
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisoi24
   use ISO_C_BINDING

   interface
      integer(C_INT_FAST16_T) function fnt1(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T) :: a
      end function fnt1
      integer(C_INT_FAST16_T) function fnt2(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), value :: a
      end function fnt2
      integer(C_INT_FAST16_T) function fnt3(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), intent(in) :: a
      end function fnt3
      integer(C_INT_FAST16_T) function fnt4(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), intent(in), value :: a
      end function fnt4
   end interface

   integer(C_INT_FAST16_T) :: a, ret

!! Test 1

   a = 5

   ret = fnt1(a)

   if ( a /= 10 ) error stop 20

!! Test 2

   a = 5

   ret = fnt2(a)

   if ( a /= 5 ) error stop 22

!! Test 3

   a = 5

   ret = fnt3(a)

   if ( a /= 5 ) error stop 24

!! Test 4

   a = 5

   ret = fnt4(a)

   if ( a /= 5 ) error stop 26

end program fxisoi24

integer(C_INT_FAST16_T) function fnt1(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T) :: a

   if ( a /= 5 ) error stop 28

   a = a + 5

   fnt1 = a**2
end function fnt1

integer(C_INT_FAST16_T) function fnt2(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), value :: a

   if ( a /= 5 ) error stop 30

   a = a + 5

   fnt2 = a**2
end function fnt2

integer(C_INT_FAST16_T) function fnt3(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in) :: a

   if ( a /= 5 ) error stop 32

   fnt3 = a**2
end function fnt3

integer(C_INT_FAST16_T) function fnt4(a)
   use ISO_C_BINDING

   integer(C_INT_FAST16_T), intent(in), value :: a

   if ( a /= 5 ) error stop 34

   fnt4 = a**2
end function fnt4
