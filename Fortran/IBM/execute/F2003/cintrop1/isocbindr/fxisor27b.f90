!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa01.presh fxisor27b
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
!*  KEYWORD(S)                 : C_BOOL
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_BOOL
!*      - FORTRAN code only
!*      - passing derived types with 2-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob27b
   use ISO_C_BINDING

   type, bind(c) :: dtd0
      logical(C_BOOL) :: a(10,5)
   end type

   type, bind(c) :: dtd1
      logical(C_BOOL) :: a(10,5)
      type(dtd0) :: d0
   end type

   type, bind(c) :: dtd2
      logical(C_BOOL) :: a(10,5)
      type(dtd1) :: d1
   end type

end module mxisob27b

program fxisor27b
   use ISO_C_BINDING
   use mxisob27b

   interface
      logical(C_BOOL) function fnt1(dt) bind(c)
         use mxisob27b
         type(dtd0), intent(inout) :: dt
      end function fnt1
      logical(C_BOOL) function fnt2(dt) bind(c)
         use mxisob27b
         type(dtd0), value :: dt
      end function fnt2
      logical(C_BOOL) function fnt3(dt) bind(c)
         use mxisob27b
         type(dtd1), intent(inout) :: dt
      end function fnt3
      logical(C_BOOL) function fnt4(dt) bind(c)
         use mxisob27b
         type(dtd1), value :: dt
      end function fnt4
      logical(C_BOOL) function fnt5(dt) bind(c)
         use mxisob27b
         type(dtd2), intent(inout) :: dt
      end function fnt5
      logical(C_BOOL) function fnt6(dt) bind(c)
         use mxisob27b
         type(dtd2), value :: dt
      end function fnt6
      logical(C_BOOL) function fnt7(dt) bind(c)
         use mxisob27b
         type(dtd0), intent(in) :: dt
      end function fnt7
      logical(C_BOOL) function fnt8(dt) bind(c)
         use mxisob27b
         type(dtd0), intent(in), value :: dt
      end function fnt8
      logical(C_BOOL) function fnt9(dt) bind(c)
         use mxisob27b
         type(dtd1), intent(in) :: dt
      end function fnt9
      logical(C_BOOL) function fnt10(dt) bind(c)
         use mxisob27b
         type(dtd1), intent(in), value :: dt
      end function fnt10
      logical(C_BOOL) function fnt11(dt) bind(c)
         use mxisob27b
         type(dtd2), intent(in) :: dt
      end function fnt11
      logical(C_BOOL) function fnt12(dt) bind(c)
         use mxisob27b
         type(dtd2), intent(in), value :: dt
      end function fnt12
      logical(C_BOOL) function fnt13(dt) bind(c)
         use mxisob27b
         type(dtd0), intent(out) :: dt
      end function fnt13
      logical(C_BOOL) function fnt14(dt) bind(c)
         use mxisob27b
         type(dtd1), intent(out) :: dt
      end function fnt14
      logical(C_BOOL) function fnt15(dt) bind(c)
         use mxisob27b
         type(dtd2), intent(out) :: dt
      end function fnt15
   end interface

   type(dtd0) :: dta
   type(dtd1) :: dtb
   type(dtd2) :: dtc
   integer i, j
   logical ret

!! Test 1

   call initdtd0(dta)

   ret = fnt1(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) .eqv. .true. ) error stop 20
      end do
   end do


!! Test 2

   call initdtd0(dta)

   ret = fnt2(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) .neqv. .true. ) error stop 22
      end do
   end do


!! Test 3

   call initdtd1(dtb)

   ret = fnt3(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) .eqv. .true. ) error stop 24
         if ( dtb%d0%a(j,i) .eqv. .true. ) error stop 26
      end do
   end do


!! Test 4

   call initdtd1(dtb)

   ret = fnt4(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) .neqv. .true. ) error stop 28
         if ( dtb%d0%a(j,i) .neqv. .true. ) error stop 30
      end do
   end do


!! Test 5

   call initdtd2(dtc)

   ret = fnt5(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) .eqv. .true. ) error stop 32
         if ( dtc%d1%a(j,i) .eqv. .true. ) error stop 34
         if ( dtc%d1%d0%a(j,i) .eqv. .true. ) error stop 36
      end do
   end do


!! Test 6

   call initdtd2(dtc)

   ret = fnt6(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) .neqv. .true. ) error stop 38
         if ( dtc%d1%a(j,i) .neqv. .true. ) error stop 40
         if ( dtc%d1%d0%a(j,i) .neqv. .true. ) error stop 42
      end do
   end do


!! Test 7

   call initdtd0(dta)

   ret = fnt7(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) .neqv. .true. ) error stop 44
      end do
   end do


!! Test 8

   call initdtd0(dta)

   ret = fnt8(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) .neqv. .true. ) error stop 46
      end do
   end do


!! Test 9

   call initdtd1(dtb)

   ret = fnt9(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) .neqv. .true. ) error stop 48
         if ( dtb%d0%a(j,i) .neqv. .true. ) error stop 50
      end do
   end do


!! Test 10

   call initdtd1(dtb)

   ret = fnt10(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) .neqv. .true. ) error stop 52
         if ( dtb%d0%a(j,i) .neqv. .true. ) error stop 54
      end do
   end do


!! Test 11

   call initdtd2(dtc)

   ret = fnt11(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) .neqv. .true. ) error stop 56
         if ( dtc%d1%a(j,i) .neqv. .true. ) error stop 58
         if ( dtc%d1%d0%a(j,i) .neqv. .true. ) error stop 60
      end do
   end do


!! Test 12

   call initdtd2(dtc)

   ret = fnt12(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) .neqv. .true. ) error stop 62
         if ( dtc%d1%a(j,i) .neqv. .true. ) error stop 64
         if ( dtc%d1%d0%a(j,i) .neqv. .true. ) error stop 66
      end do
   end do


!! Test 13

   call initdtd0(dta)

   ret = fnt13(dta)

   do i = 1, 5
      do j = 1, 10
         if ( dta%a(j,i) .eqv. .true. ) error stop 68
      end do
   end do


!! Test 14

   call initdtd1(dtb)

   ret = fnt14(dtb)

   do i = 1, 5
      do j = 1, 10
         if ( dtb%a(j,i) .eqv. .true. ) error stop 70
         if ( dtb%d0%a(j,i) .eqv. .true. ) error stop 72
      end do
   end do


!! Test 15

   call initdtd2(dtc)

   ret = fnt15(dtc)

   do i = 1, 5
      do j = 1, 10
         if ( dtc%a(j,i) .eqv. .true. ) error stop 74
         if ( dtc%d1%a(j,i) .eqv. .true. ) error stop 76
         if ( dtc%d1%d0%a(j,i) .eqv. .true. ) error stop 78
      end do
   end do


end program fxisor27b

subroutine initdtd0(dt)
   use mxisob27b

   type(dtd0) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = .true.
      end do
   end do


end subroutine initdtd0

subroutine initdtd1(dt)
   use mxisob27b

   type(dtd1) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = .true.
      end do
   end do


   call initdtd0(dt%d0)

end subroutine initdtd1

subroutine initdtd2(dt)
   use mxisob27b

   type(dtd2) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = .true.
      end do
   end do


   call initdtd1(dt%d1)

end subroutine initdtd2

logical(C_BOOL) function fnt1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd0), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 80
         dt%a(j,i) = .not. dt%a(j,i)
      end do
   end do


   fnt1 = .false.
end function fnt1

logical(C_BOOL) function fnt2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd0), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 82
         dt%a(j,i) = .not. dt%a(j,i)
      end do
   end do


   fnt2 = .false.
end function fnt2

logical(C_BOOL) function fnt3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd1), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 84
         dt%a(j,i) = .not. dt%a(j,i)
         if ( dt%d0%a(j,i) .neqv. .true. ) error stop 86
         dt%d0%a(j,i) = .not. dt%d0%a(j,i)
      end do
   end do


   fnt3 = .false.
end function fnt3

logical(C_BOOL) function fnt4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd1), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 88
         dt%a(j,i) = .not. dt%a(j,i)
         if ( dt%d0%a(j,i) .neqv. .true. ) error stop 90
         dt%d0%a(j,i) = .not. dt%d0%a(j,i)
      end do
   end do


   fnt4 = .false.
end function fnt4

logical(C_BOOL) function fnt5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd2), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 92
         dt%a(j,i) = .not. dt%a(j,i)
         if ( dt%d1%a(j,i) .neqv. .true. ) error stop 94
         dt%d1%a(j,i) = .not. dt%d1%a(j,i)
         if ( dt%d1%d0%a(j,i) .neqv. .true. ) error stop 96
         dt%d1%d0%a(j,i) = .not. dt%d1%d0%a(j,i)
      end do
   end do


   fnt5 = .false.
end function fnt5

logical(C_BOOL) function fnt6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd2), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 98
         dt%a(j,i) = .not. dt%a(j,i)
         if ( dt%d1%a(j,i) .neqv. .true. ) error stop 100
         dt%d1%a(j,i) = .not. dt%d1%a(j,i)
         if ( dt%d1%d0%a(j,i) .neqv. .true. ) error stop 102
         dt%d1%d0%a(j,i) = .not. dt%d1%d0%a(j,i)
      end do
   end do


   fnt6 = .false.
end function fnt6

logical(C_BOOL) function fnt7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd0), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 104
      end do
   end do


   fnt7 = .false.
end function fnt7

logical(C_BOOL) function fnt8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd0), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 106
      end do
   end do


   fnt8 = .false.
end function fnt8

logical(C_BOOL) function fnt9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd1), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 108
         if ( dt%d0%a(j,i) .neqv. .true. ) error stop 110
      end do
   end do


   fnt9 = .false.
end function fnt9

logical(C_BOOL) function fnt10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd1), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 112
         if ( dt%d0%a(j,i) .neqv. .true. ) error stop 114
      end do
   end do


   fnt10 = .false.
end function fnt10

logical(C_BOOL) function fnt11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd2), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 116
         if ( dt%d1%a(j,i) .neqv. .true. ) error stop 118
         if ( dt%d1%d0%a(j,i) .neqv. .true. ) error stop 120
      end do
   end do


   fnt11 = .false.
end function fnt11

logical(C_BOOL) function fnt12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd2), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 122
         if ( dt%d1%a(j,i) .neqv. .true. ) error stop 124
         if ( dt%d1%d0%a(j,i) .neqv. .true. ) error stop 126
      end do
   end do


   fnt12 = .false.
end function fnt12

logical(C_BOOL) function fnt13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd0), intent(out) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = .not. dt%a(j,i)
      end do
   end do


   fnt13 = .false.
end function fnt13

logical(C_BOOL) function fnt14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd1), intent(out) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = .not. dt%a(j,i)
         dt%d0%a(j,i) = .not. dt%d0%a(j,i)
      end do
   end do


   fnt14 = .false.
end function fnt14

logical(C_BOOL) function fnt15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27b

   type(dtd2), intent(out) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = .not. dt%a(j,i)
         dt%d1%a(j,i) = .not. dt%d1%a(j,i)
         dt%d1%d0%a(j,i) = .not. dt%d1%d0%a(j,i)
      end do
   end do


   fnt15 = .false.
end function fnt15
