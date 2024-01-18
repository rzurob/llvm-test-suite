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
!*  KEYWORD(S)                 : C_FLOAT, C_DOUBLE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_FLOAT and C_DOUBLE
!*      - FORTRAN code only
!*      - passing scalar arguments by REFERENCE and by VALUE
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisol24
   use ISO_C_BINDING

   interface
      real(C_FLOAT) function fnt1(a,b)
         use ISO_C_BINDING
         real(C_FLOAT) :: a
         real(C_DOUBLE) :: b
      end function fnt1
      real(C_FLOAT) function fnt2(a,b)
         use ISO_C_BINDING
         real(C_FLOAT), value :: a
         real(C_DOUBLE), value :: b
      end function fnt2
      real(C_FLOAT) function fnt3(a,b)
         use ISO_C_BINDING
         real(C_FLOAT), intent(in) :: a
         real(C_DOUBLE), intent(in) :: b
      end function fnt3
      real(C_FLOAT) function fnt4(a,b)
         use ISO_C_BINDING
         real(C_FLOAT), intent(in), value :: a
         real(C_DOUBLE), intent(in), value :: b
      end function fnt4
   end interface

   real(C_FLOAT) :: a, ret
   real(C_DOUBLE) :: b

!! Test 1

   a = 5.0e0
   b = 10.0d0

   ret = fnt1(a,b)

   if ( a /= 10.0e0 ) error stop 20
   if ( b /= 20.0d0 ) error stop 22

!! Test 2

   a = 5.0e0
   b = 10.0d0

   ret = fnt2(a,b)

   if ( a /= 5.0e0 ) error stop 24
   if ( b /= 10.0d0 ) error stop 26

!! Test 3

   a = 5.0e0
   b = 10.0d0

   ret = fnt3(a,b)

   if ( a /= 5.0e0 ) error stop 28
   if ( b /= 10.0d0 ) error stop 30

!! Test 4

   a = 5.0e0
   b = 10.0d0

   ret = fnt4(a,b)

   if ( a /= 5.0e0 ) error stop 32
   if ( b /= 10.0d0 ) error stop 34

end program fxisol24

real(C_FLOAT) function fnt1(a,b)
   use ISO_C_BINDING

   real(C_FLOAT) :: a
   real(C_DOUBLE) :: b

   if ( a /= 5.0e0 ) error stop 36
   if ( b /= 10.0d0 ) error stop 38

   a = a + 5.0e0
   b = b + 10.0d0

   fnt1 = 0
end function fnt1

real(C_FLOAT) function fnt2(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), value :: a
   real(C_DOUBLE), value :: b

   if ( a /= 5.0e0 ) error stop 40
   if ( b /= 10.0d0 ) error stop 42

   a = a + 5.0e0
   b = b + 10.0d0

   fnt2 = 0
end function fnt2

real(C_FLOAT) function fnt3(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), intent(in) :: a
   real(C_DOUBLE), intent(in) :: b

   if ( a /= 5.0e0 ) error stop 44
   if ( b /= 10.0d0 ) error stop 46

   fnt3 = 0
end function fnt3

real(C_FLOAT) function fnt4(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), intent(in), value :: a
   real(C_DOUBLE), intent(in), value :: b

   if ( a /= 5.0e0 ) error stop 48
   if ( b /= 10.0d0 ) error stop 50

   fnt4 = 0
end function fnt4
