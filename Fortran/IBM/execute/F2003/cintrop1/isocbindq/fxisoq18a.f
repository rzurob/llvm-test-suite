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
!*	- testing 16
!*	- using external FORTRAN functions
!*	- passing derived types with 1-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob18a
   use ISO_C_BINDING

   type, bind(c) :: dts0
      complex(16) :: a(5)
   end type

   type, bind(c) :: dts1
      complex(16) :: a(5)
      type(dts0) :: d0
   end type

   type, bind(c) :: dts2
      complex(16) :: a(5)
      type(dts1) :: d1
   end type

end module mxisob18a

complex(16) function fnt1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts0), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 20
      dt%a(i) = cmplx(i+1,i+1,16)
   end do


   fnt1 = 0
end function fnt1

complex(16) function fnt2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts0), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 22
      dt%a(i) = cmplx(i+1,i+1,16)
   end do


   fnt2 = 0
end function fnt2

complex(16) function fnt3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts1), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 24
      dt%a(i) = dt%a(i) + cmplx(i,i,16)
      if ( dt%d0%a(i) /= cmplx(i,i,16) ) error stop 26
      dt%d0%a(i) = dt%d0%a(i) + cmplx(i,i,16)
   end do


   fnt3 = 0
end function fnt3

complex(16) function fnt4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts1), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 28
      dt%a(i) = dt%a(i) + cmplx(i,i,16)
      if ( dt%d0%a(i) /= cmplx(i,i,16) ) error stop 30
      dt%d0%a(i) = dt%d0%a(i) + cmplx(i,i,16)
   end do


   fnt4 = 0
end function fnt4

complex(16) function fnt5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts2), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 32
      dt%a(i) = dt%a(i) + cmplx(i,i,16)
      if ( dt%d1%a(i) /= cmplx(i,i,16) ) error stop 34
      dt%d1%a(i) = dt%d1%a(i) + cmplx(i,i,16)
      if ( dt%d1%d0%a(i) /= cmplx(i,i,16) ) error stop 36
      dt%d1%d0%a(i) = dt%d1%d0%a(i) + cmplx(i,i,16)
   end do


   fnt5 = 0
end function fnt5

complex(16) function fnt6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts2), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 38
      dt%a(i) = dt%a(i) + cmplx(i,i,16)
      if ( dt%d1%a(i) /= cmplx(i,i,16) ) error stop 40
      dt%d1%a(i) = dt%d1%a(i) + cmplx(i,i,16)
      if ( dt%d1%d0%a(i) /= cmplx(i,i,16) ) error stop 42
      dt%d1%d0%a(i) = dt%d1%d0%a(i) + cmplx(i,i,16)
   end do


   fnt6 = 0
end function fnt6

complex(16) function fnt7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts0), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 44
   end do


   fnt7 = 0
end function fnt7

complex(16) function fnt7a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts0), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 46
   end do


   fnt7a = 0
end function fnt7a

complex(16) function fnt8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts0), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 48
   end do


   fnt8 = 0
end function fnt8

complex(16) function fnt8a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts0), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 50
   end do


   fnt8a = 0
end function fnt8a

complex(16) function fnt9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts1), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 52
      if ( dt%d0%a(i) /= cmplx(i,i,16) ) error stop 54
   end do


   fnt9 = 0
end function fnt9

complex(16) function fnt9a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts1), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 56
      if ( dt%d0%a(i) /= cmplx(i,i,16) ) error stop 58
   end do


   fnt9a = 0
end function fnt9a

complex(16) function fnt10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts1), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 60
      if ( dt%d0%a(i) /= cmplx(i,i,16) ) error stop 62
   end do


   fnt10 = 0
end function fnt10

complex(16) function fnt10a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts1), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 64
      if ( dt%d0%a(i) /= cmplx(i,i,16) ) error stop 66
   end do


   fnt10a = 0
end function fnt10a

complex(16) function fnt11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts2), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 68
      if ( dt%d1%a(i) /= cmplx(i,i,16) ) error stop 70
      if ( dt%d1%d0%a(i) /= cmplx(i,i,16) ) error stop 72
   end do


   fnt11 = 0
end function fnt11

complex(16) function fnt11a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts2), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 74
      if ( dt%d1%a(i) /= cmplx(i,i,16) ) error stop 76
      if ( dt%d1%d0%a(i) /= cmplx(i,i,16) ) error stop 78
   end do


   fnt11a = 0
end function fnt11a

complex(16) function fnt12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts2), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 80
      if ( dt%d1%a(i) /= cmplx(i,i,16) ) error stop 82
      if ( dt%d1%d0%a(i) /= cmplx(i,i,16) ) error stop 84
   end do


   fnt12 = 0
end function fnt12

complex(16) function fnt12a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts2), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,16) ) error stop 86
      if ( dt%d1%a(i) /= cmplx(i,i,16) ) error stop 88
      if ( dt%d1%d0%a(i) /= cmplx(i,i,16) ) error stop 90
   end do


   fnt12a = 0
end function fnt12a

complex(16) function fnt13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts0), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = cmplx(i+1,i+1,16)
   end do


   fnt13 = 0
end function fnt13

complex(16) function fnt14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts1), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = dt%a(i) + cmplx(i,i,16)
      dt%d0%a(i) = dt%d0%a(i) + cmplx(i,i,16)
   end do


   fnt14 = 0
end function fnt14

complex(16) function fnt15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18a

   type(dts2), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = dt%a(i) + cmplx(i,i,16)
      dt%d1%a(i) = dt%d1%a(i) + cmplx(i,i,16)
      dt%d1%d0%a(i) = dt%d1%d0%a(i) + cmplx(i,i,16)
   end do


   fnt15 = 0
end function fnt15
