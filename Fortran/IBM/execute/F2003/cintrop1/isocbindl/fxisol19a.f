!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisol19a cxisol19a
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
!*  KEYWORD(S)                 : C_FLOAT, C_DOUBLE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*
!*	- testing C_FLOAT and C_DOUBLE
!*	- using external FORTRAN subroutines
!*	- passing derived types with 1-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob19a
   use ISO_C_BINDING

   type, bind(c) :: dts0
      real(C_FLOAT) :: a(5)
      real(C_DOUBLE) :: b(3)
   end type

   type, bind(c) :: dts1
      real(C_FLOAT) :: a(5)
      real(C_DOUBLE) :: b(3)
      type(dts0) :: d0
   end type

   type, bind(c) :: dts2
      real(C_FLOAT) :: a(5)
      real(C_DOUBLE) :: b(3)
      type(dts1) :: d1
   end type

end module mxisob19a

subroutine sub1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 20
      dt%a(i) = real(i+1,C_FLOAT)
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 22
      dt%b(i) = real(i+1,C_DOUBLE)
   end do

end subroutine sub1

subroutine sub2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 24
      dt%a(i) = real(i+1,C_FLOAT)
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 26
      dt%b(i) = real(i+1,C_DOUBLE)
   end do

end subroutine sub2

subroutine sub3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 28
      dt%a(i) = real(dt%a(i) + i,C_FLOAT)
      if ( dt%d0%a(i) /= real(i,C_FLOAT) ) error stop 30
      dt%d0%a(i) = real(dt%d0%a(i) + i,C_FLOAT)
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 32
      dt%b(i) = real(dt%b(i) + i,C_DOUBLE)
      if ( dt%d0%b(i) /= real(i,C_DOUBLE) ) error stop 34
      dt%d0%b(i) = real(dt%d0%b(i) + i,C_DOUBLE)
   end do

end subroutine sub3

subroutine sub4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 36
      dt%a(i) = real(dt%a(i) + i,C_FLOAT)
      if ( dt%d0%a(i) /= real(i,C_FLOAT) ) error stop 38
      dt%d0%a(i) = real(dt%d0%a(i) + i,C_FLOAT)
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 40
      dt%b(i) = real(dt%b(i) + i,C_DOUBLE)
      if ( dt%d0%b(i) /= real(i,C_DOUBLE) ) error stop 42
      dt%d0%b(i) = real(dt%d0%b(i) + i,C_DOUBLE)
   end do

end subroutine sub4

subroutine sub5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 44
      dt%a(i) = real(dt%a(i) + i,C_FLOAT)
      if ( dt%d1%a(i) /= real(i,C_FLOAT) ) error stop 46
      dt%d1%a(i) = real(dt%d1%a(i) + i,C_FLOAT)
      if ( dt%d1%d0%a(i) /= real(i,C_FLOAT) ) error stop 48
      dt%d1%d0%a(i) = real(dt%d1%d0%a(i) + i,C_FLOAT)
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 50
      dt%b(i) = real(dt%b(i) + i,C_DOUBLE)
      if ( dt%d1%b(i) /= real(i,C_DOUBLE) ) error stop 52
      dt%d1%b(i) = real(dt%d1%b(i) + i,C_DOUBLE)
      if ( dt%d1%d0%b(i) /= real(i,C_DOUBLE) ) error stop 54
      dt%d1%d0%b(i) = real(dt%d1%d0%b(i) + i,C_DOUBLE)
   end do

end subroutine sub5

subroutine sub6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 56
      dt%a(i) = real(dt%a(i) + i,C_FLOAT)
      if ( dt%d1%a(i) /= real(i,C_FLOAT) ) error stop 58
      dt%d1%a(i) = real(dt%d1%a(i) + i,C_FLOAT)
      if ( dt%d1%d0%a(i) /= real(i,C_FLOAT) ) error stop 60
      dt%d1%d0%a(i) = real(dt%d1%d0%a(i) + i,C_FLOAT)
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 62
      dt%b(i) = real(dt%b(i) + i,C_DOUBLE)
      if ( dt%d1%b(i) /= real(i,C_DOUBLE) ) error stop 64
      dt%d1%b(i) = real(dt%d1%b(i) + i,C_DOUBLE)
      if ( dt%d1%d0%b(i) /= real(i,C_DOUBLE) ) error stop 66
      dt%d1%d0%b(i) = real(dt%d1%d0%b(i) + i,C_DOUBLE)
   end do

end subroutine sub6

subroutine sub7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 68
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 70
   end do

end subroutine sub7

subroutine sub7a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 72
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 74
   end do

end subroutine sub7a

subroutine sub8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 76
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 78
   end do

end subroutine sub8

subroutine sub8a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 80
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 82
   end do

end subroutine sub8a

subroutine sub9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 84
      if ( dt%d0%a(i) /= real(i,C_FLOAT) ) error stop 86
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 88
      if ( dt%d0%b(i) /= real(i,C_DOUBLE) ) error stop 90
   end do

end subroutine sub9

subroutine sub9a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 92
      if ( dt%d0%a(i) /= real(i,C_FLOAT) ) error stop 94
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 96
      if ( dt%d0%b(i) /= real(i,C_DOUBLE) ) error stop 98
   end do

end subroutine sub9a

subroutine sub10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 100
      if ( dt%d0%a(i) /= real(i,C_FLOAT) ) error stop 102
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 104
      if ( dt%d0%b(i) /= real(i,C_DOUBLE) ) error stop 106
   end do

end subroutine sub10

subroutine sub10a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 108
      if ( dt%d0%a(i) /= real(i,C_FLOAT) ) error stop 110
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 112
      if ( dt%d0%b(i) /= real(i,C_DOUBLE) ) error stop 114
   end do

end subroutine sub10a

subroutine sub11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 116
      if ( dt%d1%a(i) /= real(i,C_FLOAT) ) error stop 118
      if ( dt%d1%d0%a(i) /= real(i,C_FLOAT) ) error stop 120
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 122
      if ( dt%d1%b(i) /= real(i,C_DOUBLE) ) error stop 124
      if ( dt%d1%d0%b(i) /= real(i,C_DOUBLE) ) error stop 126
   end do

end subroutine sub11

subroutine sub11a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 128
      if ( dt%d1%a(i) /= real(i,C_FLOAT) ) error stop 130
      if ( dt%d1%d0%a(i) /= real(i,C_FLOAT) ) error stop 132
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 134
      if ( dt%d1%b(i) /= real(i,C_DOUBLE) ) error stop 136
      if ( dt%d1%d0%b(i) /= real(i,C_DOUBLE) ) error stop 138
   end do

end subroutine sub11a

subroutine sub12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 140
      if ( dt%d1%a(i) /= real(i,C_FLOAT) ) error stop 142
      if ( dt%d1%d0%a(i) /= real(i,C_FLOAT) ) error stop 144
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 146
      if ( dt%d1%b(i) /= real(i,C_DOUBLE) ) error stop 148
      if ( dt%d1%d0%b(i) /= real(i,C_DOUBLE) ) error stop 150
   end do

end subroutine sub12

subroutine sub12a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= real(i,C_FLOAT) ) error stop 152
      if ( dt%d1%a(i) /= real(i,C_FLOAT) ) error stop 154
      if ( dt%d1%d0%a(i) /= real(i,C_FLOAT) ) error stop 156
   end do

   do i = 1, 3
      if ( dt%b(i) /= real(i,C_DOUBLE) ) error stop 158
      if ( dt%d1%b(i) /= real(i,C_DOUBLE) ) error stop 160
      if ( dt%d1%d0%b(i) /= real(i,C_DOUBLE) ) error stop 162
   end do

end subroutine sub12a

subroutine sub13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = real(i+1,C_FLOAT)
   end do

   do i = 1, 3
      dt%b(i) = real(i+1,C_DOUBLE)
   end do

end subroutine sub13

subroutine sub14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = real(dt%a(i) + i,C_FLOAT)
      dt%d0%a(i) = real(dt%d0%a(i) + i,C_FLOAT)
   end do

   do i = 1, 3
      dt%b(i) = real(dt%b(i) + i,C_DOUBLE)
      dt%d0%b(i) = real(dt%d0%b(i) + i,C_DOUBLE)
   end do

end subroutine sub14

subroutine sub15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = real(dt%a(i) + i,C_FLOAT)
      dt%d1%a(i) = real(dt%d1%a(i) + i,C_FLOAT)
      dt%d1%d0%a(i) = real(dt%d1%d0%a(i) + i,C_FLOAT)
   end do

   do i = 1, 3
      dt%b(i) = real(dt%b(i) + i,C_DOUBLE)
      dt%d1%b(i) = real(dt%d1%b(i) + i,C_DOUBLE)
      dt%d1%d0%b(i) = real(dt%d1%d0%b(i) + i,C_DOUBLE)
   end do

end subroutine sub15
