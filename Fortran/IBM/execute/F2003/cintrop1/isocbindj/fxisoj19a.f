!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisoj19a cxisoj19a
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************
!***********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Support for ISO_C_BINDING module
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquide
!*  DATE                       : 4/23/2002
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*
!*  PRIMARY FUNCTIONS TESTED   : ISO_C_BINDING module
!*  SECONDARY FUNCTIONS TESTED : see below 
!*
!*  DRIVER STANZA              : 
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : C_SIZE_T, C_INTPTR_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*
!*	- testing C_SIZE_T and C_INTPTR_T
!*	- using external FORTRAN subroutines
!*	- passing derived types with 1-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob19a
   use ISO_C_BINDING

   type, bind(c) :: dts0
      integer(C_SIZE_T) :: a(5)
      integer(C_INTPTR_T) :: b(3)
   end type

   type, bind(c) :: dts1
      integer(C_SIZE_T) :: a(5)
      integer(C_INTPTR_T) :: b(3)
      type(dts0) :: d0
   end type

   type, bind(c) :: dts2
      integer(C_SIZE_T) :: a(5)
      integer(C_INTPTR_T) :: b(3)
      type(dts1) :: d1
   end type

end module mxisob19a

subroutine sub1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 20
      dt%a(i) = i+1
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 22
      dt%b(i) = i+1
   end do

end subroutine sub1

subroutine sub2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 24
      dt%a(i) = i+1
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 26
      dt%b(i) = i+1
   end do

end subroutine sub2

subroutine sub3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 28
      dt%a(i) = dt%a(i) + i
      if ( dt%d0%a(i) /= i ) error stop 30
      dt%d0%a(i) = dt%d0%a(i) + i
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 32
      dt%b(i) = dt%b(i) + i
      if ( dt%d0%b(i) /= i ) error stop 34
      dt%d0%b(i) = dt%d0%b(i) + i
   end do

end subroutine sub3

subroutine sub4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 36
      dt%a(i) = dt%a(i) + i
      if ( dt%d0%a(i) /= i ) error stop 38
      dt%d0%a(i) = dt%d0%a(i) + i
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 40
      dt%b(i) = dt%b(i) + i
      if ( dt%d0%b(i) /= i ) error stop 42
      dt%d0%b(i) = dt%d0%b(i) + i
   end do

end subroutine sub4

subroutine sub5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 44
      dt%a(i) = dt%a(i) + i
      if ( dt%d1%a(i) /= i ) error stop 46
      dt%d1%a(i) = dt%d1%a(i) + i
      if ( dt%d1%d0%a(i) /= i ) error stop 48
      dt%d1%d0%a(i) = dt%d1%d0%a(i) + i
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 50
      dt%b(i) = dt%b(i) + i
      if ( dt%d1%b(i) /= i ) error stop 52
      dt%d1%b(i) = dt%d1%b(i) + i
      if ( dt%d1%d0%b(i) /= i ) error stop 54
      dt%d1%d0%b(i) = dt%d1%d0%b(i) + i
   end do

end subroutine sub5

subroutine sub6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 56
      dt%a(i) = dt%a(i) + i
      if ( dt%d1%a(i) /= i ) error stop 58
      dt%d1%a(i) = dt%d1%a(i) + i
      if ( dt%d1%d0%a(i) /= i ) error stop 60
      dt%d1%d0%a(i) = dt%d1%d0%a(i) + i
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 62
      dt%b(i) = dt%b(i) + i
      if ( dt%d1%b(i) /= i ) error stop 64
      dt%d1%b(i) = dt%d1%b(i) + i
      if ( dt%d1%d0%b(i) /= i ) error stop 66
      dt%d1%d0%b(i) = dt%d1%d0%b(i) + i
   end do

end subroutine sub6

subroutine sub7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 68
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 70
   end do

end subroutine sub7

subroutine sub7a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 72
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 74
   end do

end subroutine sub7a

subroutine sub8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 76
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 78
   end do

end subroutine sub8

subroutine sub8a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 80
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 82
   end do

end subroutine sub8a

subroutine sub9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 84
      if ( dt%d0%a(i) /= i ) error stop 86
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 88
      if ( dt%d0%b(i) /= i ) error stop 90
   end do

end subroutine sub9

subroutine sub9a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 92
      if ( dt%d0%a(i) /= i ) error stop 94
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 96
      if ( dt%d0%b(i) /= i ) error stop 98
   end do

end subroutine sub9a

subroutine sub10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 100
      if ( dt%d0%a(i) /= i ) error stop 102
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 104
      if ( dt%d0%b(i) /= i ) error stop 106
   end do

end subroutine sub10

subroutine sub10a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 108
      if ( dt%d0%a(i) /= i ) error stop 110
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 112
      if ( dt%d0%b(i) /= i ) error stop 114
   end do

end subroutine sub10a

subroutine sub11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 116
      if ( dt%d1%a(i) /= i ) error stop 118
      if ( dt%d1%d0%a(i) /= i ) error stop 120
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 122
      if ( dt%d1%b(i) /= i ) error stop 124
      if ( dt%d1%d0%b(i) /= i ) error stop 126
   end do

end subroutine sub11

subroutine sub11a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 128
      if ( dt%d1%a(i) /= i ) error stop 130
      if ( dt%d1%d0%a(i) /= i ) error stop 132
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 134
      if ( dt%d1%b(i) /= i ) error stop 136
      if ( dt%d1%d0%b(i) /= i ) error stop 138
   end do

end subroutine sub11a

subroutine sub12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 140
      if ( dt%d1%a(i) /= i ) error stop 142
      if ( dt%d1%d0%a(i) /= i ) error stop 144
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 146
      if ( dt%d1%b(i) /= i ) error stop 148
      if ( dt%d1%d0%b(i) /= i ) error stop 150
   end do

end subroutine sub12

subroutine sub12a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= i ) error stop 152
      if ( dt%d1%a(i) /= i ) error stop 154
      if ( dt%d1%d0%a(i) /= i ) error stop 156
   end do

   do i = 1, 3
      if ( dt%b(i) /= i ) error stop 158
      if ( dt%d1%b(i) /= i ) error stop 160
      if ( dt%d1%d0%b(i) /= i ) error stop 162
   end do

end subroutine sub12a

subroutine sub13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = i+1
   end do

   do i = 1, 3
      dt%b(i) = i+1
   end do

end subroutine sub13

subroutine sub14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = dt%a(i) + i
      dt%d0%a(i) = dt%d0%a(i) + i
   end do

   do i = 1, 3
      dt%b(i) = dt%b(i) + i
      dt%d0%b(i) = dt%d0%b(i) + i
   end do

end subroutine sub14

subroutine sub15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = dt%a(i) + i
      dt%d1%a(i) = dt%d1%a(i) + i
      dt%d1%d0%a(i) = dt%d1%d0%a(i) + i
   end do

   do i = 1, 3
      dt%b(i) = dt%b(i) + i
      dt%d1%b(i) = dt%d1%b(i) + i
      dt%d1%d0%b(i) = dt%d1%d0%b(i) + i
   end do

end subroutine sub15
