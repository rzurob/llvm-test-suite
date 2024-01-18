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
!*  KEYWORD(S)                 : C_LONG_DOUBLE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_LONG_DOUBLE
!*      - FORTRAN code only
!*      - passing scalar arguments by REFERENCE and by VALUE
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisom24
   use ISO_C_BINDING

   interface
      real(C_LONG_DOUBLE) function fnt1(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE) :: a
      end function fnt1
      real(C_LONG_DOUBLE) function fnt2(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), value :: a
      end function fnt2
      real(C_LONG_DOUBLE) function fnt3(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), intent(in) :: a
      end function fnt3
      real(C_LONG_DOUBLE) function fnt4(a)
         use ISO_C_BINDING
         real(C_LONG_DOUBLE), intent(in), value :: a
      end function fnt4
   end interface

   real(C_LONG_DOUBLE) :: a, ret

!! Test 1

   a = 5.0d0

   ret = fnt1(a)

   if ( a /= 10.0d0 ) error stop 20

!! Test 2

   a = 5.0d0

   ret = fnt2(a)

   if ( a /= 5.0d0 ) error stop 22

!! Test 3

   a = 5.0d0

   ret = fnt3(a)

   if ( a /= 5.0d0 ) error stop 24

!! Test 4

   a = 5.0d0

   ret = fnt4(a)

   if ( a /= 5.0d0 ) error stop 26

end program fxisom24

real(C_LONG_DOUBLE) function fnt1(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE) :: a

   if ( a /= 5.0d0 ) error stop 28

   a = a + 5.0d0

   fnt1 = 0
end function fnt1

real(C_LONG_DOUBLE) function fnt2(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), value :: a

   if ( a /= 5.0d0 ) error stop 30

   a = a + 5.0d0

   fnt2 = 0
end function fnt2

real(C_LONG_DOUBLE) function fnt3(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(in) :: a

   if ( a /= 5.0d0 ) error stop 32

   fnt3 = 0
end function fnt3

real(C_LONG_DOUBLE) function fnt4(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(in), value :: a

   if ( a /= 5.0d0 ) error stop 34

   fnt4 = 0
end function fnt4
