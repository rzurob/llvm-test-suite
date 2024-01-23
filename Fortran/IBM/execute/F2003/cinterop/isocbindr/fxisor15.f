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
!*      - testing C_BOOL
!*      - using C functions with interfaces to FORTRAN functions
!*      - function interfaces defined in module
!*      - passing derived types with scalar fields as arguments
!*      - testing INTENT and VALUE attributes
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob15
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

end module mxisob15

module mxisob15a
   interface
      logical(C_BOOL) function fnt1(dt) bind(c)
         use mxisob15
         type(dt0), intent(inout) :: dt
      end function fnt1
      logical(C_BOOL) function fnt2(dt) bind(c)
         use mxisob15
         type(dt0), value :: dt
      end function fnt2
      logical(C_BOOL) function fnt3(dt) bind(c)
         use mxisob15
         type(dt1), intent(inout) :: dt
      end function fnt3
      logical(C_BOOL) function fnt4(dt) bind(c)
         use mxisob15
         type(dt1), value :: dt
      end function fnt4
      logical(C_BOOL) function fnt5(dt) bind(c)
         use mxisob15
         type(dt2), intent(inout) :: dt
      end function fnt5
      logical(C_BOOL) function fnt6(dt) bind(c)
         use mxisob15
         type(dt2), value :: dt
      end function fnt6
      logical(C_BOOL) function fnt7(dt) bind(c)
         use mxisob15
         type(dt0), intent(in) :: dt
      end function fnt7
      logical(C_BOOL) function fnt7a(dt) bind(c)
         use mxisob15
         type(dt0), intent(in) :: dt
      end function fnt7a
      logical(C_BOOL) function fnt8(dt) bind(c)
         use mxisob15
         type(dt0), intent(in), value :: dt
      end function fnt8
      logical(C_BOOL) function fnt8a(dt) bind(c)
         use mxisob15
         type(dt0), intent(in), value :: dt
      end function fnt8a
      logical(C_BOOL) function fnt9(dt) bind(c)
         use mxisob15
         type(dt1), intent(in) :: dt
      end function fnt9
      logical(C_BOOL) function fnt9a(dt) bind(c)
         use mxisob15
         type(dt1), intent(in) :: dt
      end function fnt9a
      logical(C_BOOL) function fnt10(dt) bind(c)
         use mxisob15
         type(dt1), intent(in), value :: dt
      end function fnt10
      logical(C_BOOL) function fnt10a(dt) bind(c)
         use mxisob15
         type(dt1), intent(in), value :: dt
      end function fnt10a
      logical(C_BOOL) function fnt11(dt) bind(c)
         use mxisob15
         type(dt2), intent(in) :: dt
      end function fnt11
      logical(C_BOOL) function fnt11a(dt) bind(c)
         use mxisob15
         type(dt2), intent(in) :: dt
      end function fnt11a
      logical(C_BOOL) function fnt12(dt) bind(c)
         use mxisob15
         type(dt2), intent(in), value :: dt
      end function fnt12
      logical(C_BOOL) function fnt12a(dt) bind(c)
         use mxisob15
         type(dt2), intent(in), value :: dt
      end function fnt12a
      logical(C_BOOL) function fnt13(dt) bind(c)
         use mxisob15
         type(dt0), intent(out) :: dt
      end function fnt13
      logical(C_BOOL) function fnt14(dt) bind(c)
         use mxisob15
         type(dt1), intent(out) :: dt
      end function fnt14
      logical(C_BOOL) function fnt15(dt) bind(c)
         use mxisob15
         type(dt2), intent(out) :: dt
      end function fnt15
   end interface
end module mxisob15a

program fxisor15
   use ISO_C_BINDING
   use mxisob15
   use mxisob15a

   type(dt0) :: dta
   type(dt1) :: dtb
   type(dt2) :: dtc
   logical ret

!! Test 1

   dta%a = .true.

   ret = fnt1(dta)

   if ( dta%a .eqv. .true. ) error stop 20

!! Test 2

   dta%a = .true.

   ret = fnt2(dta)

   if ( dta%a .neqv. .true. ) error stop 22

!! Test 3

   dtb%a = .true.
   dtb%d0%a = .true.

   ret = fnt3(dtb)

   if ( dtb%a .eqv. .true. ) error stop 24
   if ( dtb%d0%a .eqv. .true. ) error stop 26

!! Test 4

   dtb%a = .true.
   dtb%d0%a = .true.

   ret = fnt4(dtb)

   if ( dtb%a .neqv. .true. ) error stop 28
   if ( dtb%d0%a .neqv. .true. ) error stop 30

!! Test 5

   dtc%a = .true.
   dtc%d1%a = .true.
   dtc%d1%d0%a = .true.

   ret = fnt5(dtc)

   if ( dtc%a .eqv. .true. ) error stop 32
   if ( dtc%d1%a .eqv. .true. ) error stop 34
   if ( dtc%d1%d0%a .eqv. .true. ) error stop 36

!! Test 6

   dtc%a = .true.
   dtc%d1%a = .true.
   dtc%d1%d0%a = .true.

   ret = fnt6(dtc)

   if ( dtc%a .neqv. .true. ) error stop 38
   if ( dtc%d1%a .neqv. .true. ) error stop 40
   if ( dtc%d1%d0%a .neqv. .true. ) error stop 42

!! Test 7

   dta%a = .true.

   ret = fnt7(dta)

   if ( dta%a .neqv. .true. ) error stop 44

!! Test 7a

   dta%a = .true.

   ret = fnt7a(dta)

   if ( dta%a .neqv. .true. ) error stop 46

!! Test 8

   dta%a = .true.

   ret = fnt8(dta)

   if ( dta%a .neqv. .true. ) error stop 48

!! Test 8a

   dta%a = .true.

   ret = fnt8a(dta)

   if ( dta%a .neqv. .true. ) error stop 50

!! Test 9

   dtb%a = .true.
   dtb%d0%a = .true.

   ret = fnt9(dtb)

   if ( dtb%a .neqv. .true. ) error stop 52
   if ( dtb%d0%a .neqv. .true. ) error stop 54

!! Test 9a

   dtb%a = .true.
   dtb%d0%a = .true.

   ret = fnt9a(dtb)

   if ( dtb%a .neqv. .true. ) error stop 56
   if ( dtb%d0%a .neqv. .true. ) error stop 58

!! Test 10

   dtb%a = .true.
   dtb%d0%a = .true.

   ret = fnt10(dtb)

   if ( dtb%a .neqv. .true. ) error stop 60
   if ( dtb%d0%a .neqv. .true. ) error stop 62

!! Test 10a

   dtb%a = .true.
   dtb%d0%a = .true.

   ret = fnt10a(dtb)

   if ( dtb%a .neqv. .true. ) error stop 64
   if ( dtb%d0%a .neqv. .true. ) error stop 66

!! Test 11

   dtc%a = .true.
   dtc%d1%a = .true.
   dtc%d1%d0%a = .true.

   ret = fnt11(dtc)

   if ( dtc%a .neqv. .true. ) error stop 68
   if ( dtc%d1%a .neqv. .true. ) error stop 70
   if ( dtc%d1%d0%a .neqv. .true. ) error stop 72

!! Test 11a

   dtc%a = .true.
   dtc%d1%a = .true.
   dtc%d1%d0%a = .true.

   ret = fnt11a(dtc)

   if ( dtc%a .neqv. .true. ) error stop 74
   if ( dtc%d1%a .neqv. .true. ) error stop 76
   if ( dtc%d1%d0%a .neqv. .true. ) error stop 78

!! Test 12

   dtc%a = .true.
   dtc%d1%a = .true.
   dtc%d1%d0%a = .true.

   ret = fnt12(dtc)

   if ( dtc%a .neqv. .true. ) error stop 80
   if ( dtc%d1%a .neqv. .true. ) error stop 82
   if ( dtc%d1%d0%a .neqv. .true. ) error stop 84

!! Test 12a

   dtc%a = .true.
   dtc%d1%a = .true.
   dtc%d1%d0%a = .true.

   ret = fnt12a(dtc)

   if ( dtc%a .neqv. .true. ) error stop 86
   if ( dtc%d1%a .neqv. .true. ) error stop 88
   if ( dtc%d1%d0%a .neqv. .true. ) error stop 90

!! Test 13

   dta%a = .true.

   ret = fnt13(dta)

   if ( dta%a .eqv. .true. ) error stop 92

!! Test 14

   dtb%a = .true.
   dtb%d0%a = .true.

   ret = fnt14(dtb)

   if ( dtb%a .eqv. .true. ) error stop 94
   if ( dtb%d0%a .eqv. .true. ) error stop 96

!! Test 15

   dtc%a = .true.
   dtc%d1%a = .true.
   dtc%d1%d0%a = .true.

   ret = fnt15(dtc)

   if ( dtc%a .eqv. .true. ) error stop 98
   if ( dtc%d1%a .eqv. .true. ) error stop 100
   if ( dtc%d1%d0%a .eqv. .true. ) error stop 102

end
