!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa01.presh fxisor27a
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
!*      - passing derived types with 1-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob27a
   use ISO_C_BINDING

   type, bind(c) :: dts0
      logical(C_BOOL) :: a(5)
   end type

   type, bind(c) :: dts1
      logical(C_BOOL) :: a(5)
      type(dts0) :: d0
   end type

   type, bind(c) :: dts2
      logical(C_BOOL) :: a(5)
      type(dts1) :: d1
   end type

end module mxisob27a

program fxisor27a
   use ISO_C_BINDING
   use mxisob27a

   interface
      logical(C_BOOL) function fnt1(dt) bind(c)
         use mxisob27a
         type(dts0), intent(inout) :: dt
      end function fnt1
      logical(C_BOOL) function fnt2(dt) bind(c)
         use mxisob27a
         type(dts0), value :: dt
      end function fnt2
      logical(C_BOOL) function fnt3(dt) bind(c)
         use mxisob27a
         type(dts1), intent(inout) :: dt
      end function fnt3
      logical(C_BOOL) function fnt4(dt) bind(c)
         use mxisob27a
         type(dts1), value :: dt
      end function fnt4
      logical(C_BOOL) function fnt5(dt) bind(c)
         use mxisob27a
         type(dts2), intent(inout) :: dt
      end function fnt5
      logical(C_BOOL) function fnt6(dt) bind(c)
         use mxisob27a
         type(dts2), value :: dt
      end function fnt6
      logical(C_BOOL) function fnt7(dt) bind(c)
         use mxisob27a
         type(dts0), intent(in) :: dt
      end function fnt7
      logical(C_BOOL) function fnt8(dt) bind(c)
         use mxisob27a
         type(dts0), intent(in), value :: dt
      end function fnt8
      logical(C_BOOL) function fnt9(dt) bind(c)
         use mxisob27a
         type(dts1), intent(in) :: dt
      end function fnt9
      logical(C_BOOL) function fnt10(dt) bind(c)
         use mxisob27a
         type(dts1), intent(in), value :: dt
      end function fnt10
      logical(C_BOOL) function fnt11(dt) bind(c)
         use mxisob27a
         type(dts2), intent(in) :: dt
      end function fnt11
      logical(C_BOOL) function fnt12(dt) bind(c)
         use mxisob27a
         type(dts2), intent(in), value :: dt
      end function fnt12
      logical(C_BOOL) function fnt13(dt) bind(c)
         use mxisob27a
         type(dts0), intent(out) :: dt
      end function fnt13
      logical(C_BOOL) function fnt14(dt) bind(c)
         use mxisob27a
         type(dts1), intent(out) :: dt
      end function fnt14
      logical(C_BOOL) function fnt15(dt) bind(c)
         use mxisob27a
         type(dts2), intent(out) :: dt
      end function fnt15
   end interface

   type(dts0) :: dta
   type(dts1) :: dtb
   type(dts2) :: dtc
   logical ret

!! Test 1

   call initdts0(dta)

   ret = fnt1(dta)

   do i = 1, 5
      if ( dta%a(i) .eqv. .true. ) error stop 20
   end do


!! Test 2

   call initdts0(dta)

   ret = fnt2(dta)

   do i = 1, 5
      if ( dta%a(i) .neqv. .true. ) error stop 22
   end do


!! Test 3

   call initdts1(dtb)

   ret = fnt3(dtb)

   do i = 1, 5
      if ( dtb%a(i) .eqv. .true. ) error stop 24
      if ( dtb%d0%a(i) .eqv. .true. ) error stop 26
   end do


!! Test 4

   call initdts1(dtb)

   ret = fnt4(dtb)

   do i = 1, 5
      if ( dtb%a(i) .neqv. .true. ) error stop 28
      if ( dtb%d0%a(i) .neqv. .true. ) error stop 30
   end do


!! Test 5

   call initdts2(dtc)

   ret = fnt5(dtc)

   do i = 1, 5
      if ( dtc%a(i) .eqv. .true. ) error stop 32
      if ( dtc%d1%a(i) .eqv. .true. ) error stop 34
      if ( dtc%d1%d0%a(i) .eqv. .true. ) error stop 36
   end do


!! Test 6

   call initdts2(dtc)

   ret = fnt6(dtc)

   do i = 1, 5
      if ( dtc%a(i) .neqv. .true. ) error stop 38
      if ( dtc%d1%a(i) .neqv. .true. ) error stop 40
      if ( dtc%d1%d0%a(i) .neqv. .true. ) error stop 42
   end do


!! Test 7

   call initdts0(dta)

   ret = fnt7(dta)

   do i = 1, 5
      if ( dta%a(i) .neqv. .true. ) error stop 44
   end do


!! Test 8

   call initdts0(dta)

   ret = fnt8(dta)

   do i = 1, 5
      if ( dta%a(i) .neqv. .true. ) error stop 46
   end do


!! Test 9

   call initdts1(dtb)

   ret = fnt9(dtb)

   do i = 1, 5
      if ( dtb%a(i) .neqv. .true. ) error stop 48
      if ( dtb%d0%a(i) .neqv. .true. ) error stop 50
   end do


!! Test 10

   call initdts1(dtb)

   ret = fnt10(dtb)

   do i = 1, 5
      if ( dtb%a(i) .neqv. .true. ) error stop 52
      if ( dtb%d0%a(i) .neqv. .true. ) error stop 54
   end do


!! Test 11

   call initdts2(dtc)

   ret = fnt11(dtc)

   do i = 1, 5
      if ( dtc%a(i) .neqv. .true. ) error stop 56
      if ( dtc%d1%a(i) .neqv. .true. ) error stop 58
      if ( dtc%d1%d0%a(i) .neqv. .true. ) error stop 60
   end do


!! Test 12

   call initdts2(dtc)

   ret = fnt12(dtc)

   do i = 1, 5
      if ( dtc%a(i) .neqv. .true. ) error stop 62
      if ( dtc%d1%a(i) .neqv. .true. ) error stop 64
      if ( dtc%d1%d0%a(i) .neqv. .true. ) error stop 66
   end do


!! Test 13

   call initdts0(dta)

   ret = fnt13(dta)

   do i = 1, 5
      if ( dta%a(i) .eqv. .true. ) error stop 68
   end do


!! Test 14

   call initdts1(dtb)

   ret = fnt14(dtb)

   do i = 1, 5
      if ( dtb%a(i) .eqv. .true. ) error stop 70
      if ( dtb%d0%a(i) .eqv. .true. ) error stop 72
   end do


!! Test 15

   call initdts2(dtc)

   ret = fnt15(dtc)

   do i = 1, 5
      if ( dtc%a(i) .eqv. .true. ) error stop 74
      if ( dtc%d1%a(i) .eqv. .true. ) error stop 76
      if ( dtc%d1%d0%a(i) .eqv. .true. ) error stop 78
   end do


end program fxisor27a

subroutine initdts0(dt)
   use mxisob27a

   type(dts0) :: dt

   do i = 1, 5
      dt%a(i) = .true.
   end do


end subroutine initdts0

subroutine initdts1(dt)
   use mxisob27a

   type(dts1) :: dt

   do i = 1, 5
      dt%a(i) = .true.
   end do


   call initdts0(dt%d0)

end subroutine initdts1

subroutine initdts2(dt)
   use mxisob27a

   type(dts2) :: dt

   do i = 1, 5
      dt%a(i) = .true.
   end do


   call initdts1(dt%d1)

end subroutine initdts2

logical(C_BOOL) function fnt1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts0), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 80
      dt%a(i) = .not. dt%a(i)
   end do


   fnt1 = .false.
end function fnt1

logical(C_BOOL) function fnt2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts0), value :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 82
      dt%a(i) = .not. dt%a(i)
   end do


   fnt2 = .false.
end function fnt2

logical(C_BOOL) function fnt3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts1), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 84
      dt%a(i) = .not. dt%a(i)
      if ( dt%d0%a(i) .neqv. .true. ) error stop 86
      dt%d0%a(i) = .not. dt%d0%a(i)
   end do


   fnt3 = .false.
end function fnt3

logical(C_BOOL) function fnt4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts1), value :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 88
      dt%a(i) = .not. dt%a(i)
      if ( dt%d0%a(i) .neqv. .true. ) error stop 90
      dt%d0%a(i) = .not. dt%d0%a(i)
   end do


   fnt4 = .false.
end function fnt4

logical(C_BOOL) function fnt5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts2), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 92
      dt%a(i) = .not. dt%a(i)
      if ( dt%d1%a(i) .neqv. .true. ) error stop 94
      dt%d1%a(i) = .not. dt%d1%a(i)
      if ( dt%d1%d0%a(i) .neqv. .true. ) error stop 96
      dt%d1%d0%a(i) = .not. dt%d1%d0%a(i)
   end do


   fnt5 = .false.
end function fnt5

logical(C_BOOL) function fnt6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts2), value :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 98
      dt%a(i) = .not. dt%a(i)
      if ( dt%d1%a(i) .neqv. .true. ) error stop 100
      dt%d1%a(i) = .not. dt%d1%a(i)
      if ( dt%d1%d0%a(i) .neqv. .true. ) error stop 102
      dt%d1%d0%a(i) = .not. dt%d1%d0%a(i)
   end do


   fnt6 = .false.
end function fnt6

logical(C_BOOL) function fnt7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts0), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 104
   end do


   fnt7 = .false.
end function fnt7

logical(C_BOOL) function fnt8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts0), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 106
   end do


   fnt8 = .false.
end function fnt8

logical(C_BOOL) function fnt9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts1), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 108
      if ( dt%d0%a(i) .neqv. .true. ) error stop 110
   end do


   fnt9 = .false.
end function fnt9

logical(C_BOOL) function fnt10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts1), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 112
      if ( dt%d0%a(i) .neqv. .true. ) error stop 114
   end do


   fnt10 = .false.
end function fnt10

logical(C_BOOL) function fnt11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts2), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 116
      if ( dt%d1%a(i) .neqv. .true. ) error stop 118
      if ( dt%d1%d0%a(i) .neqv. .true. ) error stop 120
   end do


   fnt11 = .false.
end function fnt11

logical(C_BOOL) function fnt12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts2), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 122
      if ( dt%d1%a(i) .neqv. .true. ) error stop 124
      if ( dt%d1%d0%a(i) .neqv. .true. ) error stop 126
   end do


   fnt12 = .false.
end function fnt12

logical(C_BOOL) function fnt13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts0), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = .not.       dt%a(i)
   end do


   fnt13 = .false.
end function fnt13

logical(C_BOOL) function fnt14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts1), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = .not. dt%a(i)
      dt%d0%a(i) = .not. dt%d0%a(i)
   end do


   fnt14 = .false.
end function fnt14

logical(C_BOOL) function fnt15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts2), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = .not. dt%a(i)
      dt%d1%a(i) = .not. dt%d1%a(i)
      dt%d1%d0%a(i) = .not. dt%d1%d0%a(i)
   end do


   fnt15 = .false.
end function fnt15
