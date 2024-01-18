!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoi00.presh fxisoi12 cxisoi12
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 4/23/2002
!*  ORIGIN                     : AIX Compiler Development,
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
!*	- testing C_INT_FAST16_T
!*	- using external FORTRAN functions
!*	- passing derived types with scalar fields as arguments
!*      - testing INTENT and VALUE attributes
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob12
   use ISO_C_BINDING

   type, bind(c) :: dt0
      integer(C_INT_FAST16_T) :: a
   end type

   type, bind(c) :: dt1
      integer(C_INT_FAST16_T) :: a
      type(dt0) :: d0
   end type

   type, bind(c) :: dt2
      integer(C_INT_FAST16_T) :: a
      type(dt1) :: d1
   end type

end module mxisob12

integer(C_INT_FAST16_T) function fnt1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(inout) :: dt

   if ( dt%a /= 5 ) error stop 20

   dt%a = dt%a + 5

   fnt1 = 0
end function fnt1

integer(C_INT_FAST16_T) function fnt2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), value :: dt

   if ( dt%a /= 5 ) error stop 22

   dt%a = dt%a + 5

   fnt2 = 0
end function fnt2

integer(C_INT_FAST16_T) function fnt3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(inout) :: dt

   if ( dt%a /= 5 ) error stop 24
   if ( dt%d0%a /= 5 ) error stop 26

   dt%a = dt%a + 5
   dt%d0%a = dt%d0%a + 5

   fnt3 = 0
end function fnt3

integer(C_INT_FAST16_T) function fnt4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), value :: dt

   if ( dt%a /= 5 ) error stop 28
   if ( dt%d0%a /= 5 ) error stop 30

   dt%a = dt%a + 5
   dt%d0%a = dt%d0%a + 5

   fnt4 = 0
end function fnt4

integer(C_INT_FAST16_T) function fnt5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(inout) :: dt

   if ( dt%a /= 5 ) error stop 32
   if ( dt%d1%a /= 5 ) error stop 34
   if ( dt%d1%d0%a /= 5 ) error stop 36

   dt%a = dt%a + 5
   dt%d1%a = dt%d1%a + 5
   dt%d1%d0%a = dt%d1%d0%a + 5

   fnt5 = 0
end function fnt5

integer(C_INT_FAST16_T) function fnt6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), value :: dt

   if ( dt%a /= 5 ) error stop 38
   if ( dt%d1%a /= 5 ) error stop 40
   if ( dt%d1%d0%a /= 5 ) error stop 42

   dt%a = dt%a + 5
   dt%d1%a = dt%d1%a + 5
   dt%d1%d0%a = dt%d1%d0%a + 5

   fnt6 = 0
end function fnt6

integer(C_INT_FAST16_T) function fnt7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(in) :: dt

   if ( dt%a /= 5 ) error stop 44

   fnt7 = 0
end function fnt7

integer(C_INT_FAST16_T) function fnt7a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(in) :: dt

   if ( dt%a /= 5 ) error stop 46

   fnt7a = 0
end function fnt7a

integer(C_INT_FAST16_T) function fnt8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(in), value :: dt

   if ( dt%a /= 5 ) error stop 48

   fnt8 = 0
end function fnt8

integer(C_INT_FAST16_T) function fnt8a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(in), value :: dt

   if ( dt%a /= 5 ) error stop 50

   fnt8a = 0
end function fnt8a

integer(C_INT_FAST16_T) function fnt9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(in) :: dt

   if ( dt%a /= 5 ) error stop 52
   if ( dt%d0%a /= 5 ) error stop 54

   fnt9 = 0
end function fnt9

integer(C_INT_FAST16_T) function fnt9a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(in) :: dt

   if ( dt%a /= 5 ) error stop 56
   if ( dt%d0%a /= 5 ) error stop 58

   fnt9a = 0
end function fnt9a

integer(C_INT_FAST16_T) function fnt10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(in), value :: dt

   if ( dt%a /= 5 ) error stop 60
   if ( dt%d0%a /= 5 ) error stop 62

   fnt10 = 0
end function fnt10

integer(C_INT_FAST16_T) function fnt10a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(in), value :: dt

   if ( dt%a /= 5 ) error stop 64
   if ( dt%d0%a /= 5 ) error stop 66

   fnt10a = 0
end function fnt10a

integer(C_INT_FAST16_T) function fnt11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(in) :: dt

   if ( dt%a /= 5 ) error stop 68
   if ( dt%d1%a /= 5 ) error stop 70
   if ( dt%d1%d0%a /= 5 ) error stop 72

   fnt11 = 0
end function fnt11

integer(C_INT_FAST16_T) function fnt11a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(in) :: dt

   if ( dt%a /= 5 ) error stop 74
   if ( dt%d1%a /= 5 ) error stop 76
   if ( dt%d1%d0%a /= 5 ) error stop 78

   fnt11a = 0
end function fnt11a

integer(C_INT_FAST16_T) function fnt12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(in), value :: dt

   if ( dt%a /= 5 ) error stop 80
   if ( dt%d1%a /= 5 ) error stop 82
   if ( dt%d1%d0%a /= 5 ) error stop 84

   fnt12 = 0
end function fnt12

integer(C_INT_FAST16_T) function fnt12a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(in), value :: dt

   if ( dt%a /= 5 ) error stop 86
   if ( dt%d1%a /= 5 ) error stop 88
   if ( dt%d1%d0%a /= 5 ) error stop 90

   fnt12a = 0
end function fnt12a

integer(C_INT_FAST16_T) function fnt13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(out) :: dt

   dt%a = dt%a + 5

   fnt13 = 0
end function fnt13

integer(C_INT_FAST16_T) function fnt14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(out) :: dt

   dt%a = dt%a + 5
   dt%d0%a = dt%d0%a + 5

   fnt14 = 0
end function fnt14

integer(C_INT_FAST16_T) function fnt15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(out) :: dt

   dt%a = dt%a + 5
   dt%d1%a = dt%d1%a + 5
   dt%d1%d0%a = dt%d1%d0%a + 5

   fnt15 = 0
end function fnt15

