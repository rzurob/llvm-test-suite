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
!*      - testing 16
!*      - FORTRAN code only
!*      - passing scalar arguments by REFERENCE and by VALUE
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisop24
   use ISO_C_BINDING

   interface
      real(16) function fnt1(a)
         use ISO_C_BINDING
         real(16) :: a
      end function fnt1
      real(16) function fnt2(a)
         use ISO_C_BINDING
         real(16), value :: a
      end function fnt2
      real(16) function fnt3(a)
         use ISO_C_BINDING
         real(16), intent(in) :: a
      end function fnt3
      real(16) function fnt4(a)
         use ISO_C_BINDING
         real(16), intent(in), value :: a
      end function fnt4
   end interface

   real(16) :: a, ret

!! Test 1

   a = 5.0q0

   ret = fnt1(a)

   if ( a /= 10.0q0 ) error stop 20

!! Test 2

   a = 5.0q0

   ret = fnt2(a)

   if ( a /= 5.0q0 ) error stop 22

!! Test 3

   a = 5.0q0

   ret = fnt3(a)

   if ( a /= 5.0q0 ) error stop 24

!! Test 4

   a = 5.0q0

   ret = fnt4(a)

   if ( a /= 5.0q0 ) error stop 26

end program fxisop24

real(16) function fnt1(a)
   use ISO_C_BINDING

   real(16) :: a

   if ( a /= 5.0q0 ) error stop 28

   a = a + 5.0q0

   fnt1 = 0
end function fnt1

real(16) function fnt2(a)
   use ISO_C_BINDING

   real(16), value :: a

   if ( a /= 5.0q0 ) error stop 30

   a = a + 5.0q0

   fnt2 = 0
end function fnt2

real(16) function fnt3(a)
   use ISO_C_BINDING

   real(16), intent(in) :: a

   if ( a /= 5.0q0 ) error stop 32

   fnt3 = 0
end function fnt3

real(16) function fnt4(a)
   use ISO_C_BINDING

   real(16), intent(in), value :: a

   if ( a /= 5.0q0 ) error stop 34

   fnt4 = 0
end function fnt4