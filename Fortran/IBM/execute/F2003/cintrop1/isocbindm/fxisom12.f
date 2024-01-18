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
!*	- testing C_LONG_DOUBLE
!*	- using external FORTRAN functions
!*	- passing derived types with scalar fields as arguments
!*      - testing INTENT and VALUE attributes
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob12
   use ISO_C_BINDING

   type, bind(c) :: dt0
      real(C_LONG_DOUBLE) :: a
   end type

   type, bind(c) :: dt1
      real(C_LONG_DOUBLE) :: a
      type(dt0) :: d0
   end type

   type, bind(c) :: dt2
      real(C_LONG_DOUBLE) :: a
      type(dt1) :: d1
   end type

end module mxisob12

real(C_LONG_DOUBLE) function fnt1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(inout) :: dt

   if ( dt%a /= 5.0d0 ) error stop 20

   dt%a = dt%a + 5.0d0

   fnt1 = 0
end function fnt1

real(C_LONG_DOUBLE) function fnt2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), value :: dt

   if ( dt%a /= 5.0d0 ) error stop 22

   dt%a = dt%a + 5.0d0

   fnt2 = 0
end function fnt2

real(C_LONG_DOUBLE) function fnt3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(inout) :: dt

   if ( dt%a /= 5.0d0 ) error stop 24
   if ( dt%d0%a /= 5.0d0 ) error stop 26

   dt%a = dt%a + 5.0d0
   dt%d0%a = dt%d0%a + 5.0d0

   fnt3 = 0
end function fnt3

real(C_LONG_DOUBLE) function fnt4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), value :: dt

   if ( dt%a /= 5.0d0 ) error stop 28
   if ( dt%d0%a /= 5.0d0 ) error stop 30

   dt%a = dt%a + 5.0d0
   dt%d0%a = dt%d0%a + 5.0d0

   fnt4 = 0
end function fnt4

real(C_LONG_DOUBLE) function fnt5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(inout) :: dt

   if ( dt%a /= 5.0d0 ) error stop 32
   if ( dt%d1%a /= 5.0d0 ) error stop 34
   if ( dt%d1%d0%a /= 5.0d0 ) error stop 36

   dt%a = dt%a + 5.0d0
   dt%d1%a = dt%d1%a + 5.0d0
   dt%d1%d0%a = dt%d1%d0%a + 5.0d0

   fnt5 = 0
end function fnt5

real(C_LONG_DOUBLE) function fnt6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), value :: dt

   if ( dt%a /= 5.0d0 ) error stop 38
   if ( dt%d1%a /= 5.0d0 ) error stop 40
   if ( dt%d1%d0%a /= 5.0d0 ) error stop 42

   dt%a = dt%a + 5.0d0
   dt%d1%a = dt%d1%a + 5.0d0
   dt%d1%d0%a = dt%d1%d0%a + 5.0d0

   fnt6 = 0
end function fnt6

real(C_LONG_DOUBLE) function fnt7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(in) :: dt

   if ( dt%a /= 5.0d0 ) error stop 44

   fnt7 = 0
end function fnt7

real(C_LONG_DOUBLE) function fnt7a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(in) :: dt

   if ( dt%a /= 5.0d0 ) error stop 46

   fnt7a = 0
end function fnt7a

real(C_LONG_DOUBLE) function fnt8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(in), value :: dt

   if ( dt%a /= 5.0d0 ) error stop 48

   fnt8 = 0
end function fnt8

real(C_LONG_DOUBLE) function fnt8a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(in), value :: dt

   if ( dt%a /= 5.0d0 ) error stop 50

   fnt8a = 0
end function fnt8a

real(C_LONG_DOUBLE) function fnt9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(in) :: dt

   if ( dt%a /= 5.0d0 ) error stop 52
   if ( dt%d0%a /= 5.0d0 ) error stop 54

   fnt9 = 0
end function fnt9

real(C_LONG_DOUBLE) function fnt9a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(in) :: dt

   if ( dt%a /= 5.0d0 ) error stop 56
   if ( dt%d0%a /= 5.0d0 ) error stop 58

   fnt9a = 0
end function fnt9a

real(C_LONG_DOUBLE) function fnt10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(in), value :: dt

   if ( dt%a /= 5.0d0 ) error stop 60
   if ( dt%d0%a /= 5.0d0 ) error stop 62

   fnt10 = 0
end function fnt10

real(C_LONG_DOUBLE) function fnt10a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(in), value :: dt

   if ( dt%a /= 5.0d0 ) error stop 64
   if ( dt%d0%a /= 5.0d0 ) error stop 66

   fnt10a = 0
end function fnt10a

real(C_LONG_DOUBLE) function fnt11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(in) :: dt

   if ( dt%a /= 5.0d0 ) error stop 68
   if ( dt%d1%a /= 5.0d0 ) error stop 70
   if ( dt%d1%d0%a /= 5.0d0 ) error stop 72

   fnt11 = 0
end function fnt11

real(C_LONG_DOUBLE) function fnt11a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(in) :: dt

   if ( dt%a /= 5.0d0 ) error stop 74
   if ( dt%d1%a /= 5.0d0 ) error stop 76
   if ( dt%d1%d0%a /= 5.0d0 ) error stop 78

   fnt11a = 0
end function fnt11a

real(C_LONG_DOUBLE) function fnt12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(in), value :: dt

   if ( dt%a /= 5.0d0 ) error stop 80
   if ( dt%d1%a /= 5.0d0 ) error stop 82
   if ( dt%d1%d0%a /= 5.0d0 ) error stop 84

   fnt12 = 0
end function fnt12

real(C_LONG_DOUBLE) function fnt12a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(in), value :: dt

   if ( dt%a /= 5.0d0 ) error stop 86
   if ( dt%d1%a /= 5.0d0 ) error stop 88
   if ( dt%d1%d0%a /= 5.0d0 ) error stop 90

   fnt12a = 0
end function fnt12a

real(C_LONG_DOUBLE) function fnt13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(out) :: dt

   dt%a = dt%a + 5.0d0

   fnt13 = 0
end function fnt13

real(C_LONG_DOUBLE) function fnt14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(out) :: dt

   dt%a = dt%a + 5.0d0
   dt%d0%a = dt%d0%a + 5.0d0

   fnt14 = 0
end function fnt14

real(C_LONG_DOUBLE) function fnt15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(out) :: dt

   dt%a = dt%a + 5.0d0
   dt%d1%a = dt%d1%a + 5.0d0
   dt%d1%d0%a = dt%d1%d0%a + 5.0d0

   fnt15 = 0
end function fnt15

