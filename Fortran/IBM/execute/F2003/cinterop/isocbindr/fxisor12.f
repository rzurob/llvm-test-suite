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
!*  KEYWORD(S)                 : C_BOOL
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_BOOL
!*	- using external FORTRAN functions
!*	- passing derived types with scalar fields as arguments
!*      - testing INTENT and VALUE attributes
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob12
   use ISO_C_BINDING

   type, bind(c) :: dt0
      logical(C_BOOL) :: a
   end type

   type, bind(c) :: dt1
      logical(C_BOOL) :: a
      type(dt0) :: d0
   end type

   type, bind(c) :: dt2
      logical(C_BOOL) :: a
      type(dt1) :: d1
   end type

end module mxisob12

logical(C_BOOL) function fnt1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(inout) :: dt

   if ( dt%a .neqv. .true. ) error stop 20

   dt%a = .false.

   fnt1 = .false.
end function fnt1

logical(C_BOOL) function fnt2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), value :: dt

   if ( dt%a .neqv. .true. ) error stop 22

   dt%a = .false.

   fnt2 = .false.
end function fnt2

logical(C_BOOL) function fnt3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(inout) :: dt

   if ( dt%a .neqv. .true. ) error stop 24
   if ( dt%d0%a .neqv. .true. ) error stop 26

   dt%a = .false.
   dt%d0%a = .false.

   fnt3 = .false.
end function fnt3

logical(C_BOOL) function fnt4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), value :: dt

   if ( dt%a .neqv. .true. ) error stop 28
   if ( dt%d0%a .neqv. .true. ) error stop 30

   dt%a = .false.
   dt%d0%a = .false.

   fnt4 = .false.
end function fnt4

logical(C_BOOL) function fnt5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(inout) :: dt

   if ( dt%a .neqv. .true. ) error stop 32
   if ( dt%d1%a .neqv. .true. ) error stop 34
   if ( dt%d1%d0%a .neqv. .true. ) error stop 36

   dt%a = .false.
   dt%d1%a = .false.
   dt%d1%d0%a = .false.

   fnt5 = .false.
end function fnt5

logical(C_BOOL) function fnt6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), value :: dt

   if ( dt%a .neqv. .true. ) error stop 38
   if ( dt%d1%a .neqv. .true. ) error stop 40
   if ( dt%d1%d0%a .neqv. .true. ) error stop 42

   dt%a = .false.
   dt%d1%a = .false.
   dt%d1%d0%a = .false.

   fnt6 = .false.
end function fnt6

logical(C_BOOL) function fnt7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(in) :: dt

   if ( dt%a .neqv. .true. ) error stop 44

   fnt7 = .false.
end function fnt7

logical(C_BOOL) function fnt7a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(in) :: dt

   if ( dt%a .neqv. .true. ) error stop 46

   fnt7a = .false.
end function fnt7a

logical(C_BOOL) function fnt8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(in), value :: dt

   if ( dt%a .neqv. .true. ) error stop 48

   fnt8 = .false.
end function fnt8

logical(C_BOOL) function fnt8a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(in), value :: dt

   if ( dt%a .neqv. .true. ) error stop 50

   fnt8a = .false.
end function fnt8a

logical(C_BOOL) function fnt9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(in) :: dt

   if ( dt%a .neqv. .true. ) error stop 52
   if ( dt%d0%a .neqv. .true. ) error stop 54

   fnt9 = .false.
end function fnt9

logical(C_BOOL) function fnt9a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(in) :: dt

   if ( dt%a .neqv. .true. ) error stop 56
   if ( dt%d0%a .neqv. .true. ) error stop 58

   fnt9a = .false.
end function fnt9a

logical(C_BOOL) function fnt10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(in), value :: dt

   if ( dt%a .neqv. .true. ) error stop 60
   if ( dt%d0%a .neqv. .true. ) error stop 62

   fnt10 = .false.
end function fnt10

logical(C_BOOL) function fnt10a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(in), value :: dt

   if ( dt%a .neqv. .true. ) error stop 64
   if ( dt%d0%a .neqv. .true. ) error stop 66

   fnt10a = .false.
end function fnt10a

logical(C_BOOL) function fnt11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(in) :: dt

   if ( dt%a .neqv. .true. ) error stop 68
   if ( dt%d1%a .neqv. .true. ) error stop 70
   if ( dt%d1%d0%a .neqv. .true. ) error stop 72

   fnt11 = .false.
end function fnt11

logical(C_BOOL) function fnt11a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(in) :: dt

   if ( dt%a .neqv. .true. ) error stop 74
   if ( dt%d1%a .neqv. .true. ) error stop 76
   if ( dt%d1%d0%a .neqv. .true. ) error stop 78

   fnt11a = .false.
end function fnt11a

logical(C_BOOL) function fnt12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(in), value :: dt

   if ( dt%a .neqv. .true. ) error stop 80
   if ( dt%d1%a .neqv. .true. ) error stop 82
   if ( dt%d1%d0%a .neqv. .true. ) error stop 84

   fnt12 = .false.
end function fnt12

logical(C_BOOL) function fnt12a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(in), value :: dt

   if ( dt%a .neqv. .true. ) error stop 86
   if ( dt%d1%a .neqv. .true. ) error stop 88
   if ( dt%d1%d0%a .neqv. .true. ) error stop 90

   fnt12a = .false.
end function fnt12a

logical(C_BOOL) function fnt13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(out) :: dt

   dt%a = .false.

   fnt13 = .false.
end function fnt13

logical(C_BOOL) function fnt14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(out) :: dt

   dt%a = .false.
   dt%d0%a = .false.

   fnt14 = .false.
end function fnt14

logical(C_BOOL) function fnt15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(out) :: dt

   dt%a = .false.
   dt%d1%a = .false.
   dt%d1%d0%a = .false.

   fnt15 = .false.
end function fnt15

