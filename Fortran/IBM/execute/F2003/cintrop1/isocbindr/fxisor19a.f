!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisor00.presh fxisor19a cxisor19a
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
!*  KEYWORD(S)                 : C_BOOL
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*
!*	- testing C_BOOL
!*	- using external FORTRAN subroutines
!*	- passing derived types with 1-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob19a
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

end module mxisob19a

subroutine sub1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 20
      dt%a(i) = .not. dt%a(i)
   end do


end subroutine sub1

subroutine sub2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), value :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 22
      dt%a(i) = .not. dt%a(i)
   end do


end subroutine sub2

subroutine sub3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 24
      dt%a(i) = .not. dt%a(i)
      if ( dt%d0%a(i) .neqv. .true. ) error stop 26
      dt%d0%a(i) = .not. dt%d0%a(i)
   end do


end subroutine sub3

subroutine sub4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), value :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 28
      dt%a(i) = .not. dt%a(i)
      if ( dt%d0%a(i) .neqv. .true. ) error stop 30
      dt%d0%a(i) = .not. dt%d0%a(i)
   end do


end subroutine sub4

subroutine sub5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 32
      dt%a(i) = .not. dt%a(i)
      if ( dt%d1%a(i) .neqv. .true. ) error stop 34
      dt%d1%a(i) = .not. dt%d1%a(i)
      if ( dt%d1%d0%a(i) .neqv. .true. ) error stop 36
      dt%d1%d0%a(i) = .not. dt%d1%d0%a(i)
   end do


end subroutine sub5

subroutine sub6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), value :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 38
      dt%a(i) = .not. dt%a(i)
      if ( dt%d1%a(i) .neqv. .true. ) error stop 40
      dt%d1%a(i) = .not. dt%d1%a(i)
      if ( dt%d1%d0%a(i) .neqv. .true. ) error stop 42
      dt%d1%d0%a(i) = .not. dt%d1%d0%a(i)
   end do


end subroutine sub6

subroutine sub7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 44
   end do


end subroutine sub7

subroutine sub7a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 46
   end do


end subroutine sub7a

subroutine sub8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 48
   end do


end subroutine sub8

subroutine sub8a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 50
   end do


end subroutine sub8a

subroutine sub9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 52
      if ( dt%d0%a(i) .neqv. .true. ) error stop 54
   end do


end subroutine sub9

subroutine sub9a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 56
      if ( dt%d0%a(i) .neqv. .true. ) error stop 58
   end do


end subroutine sub9a

subroutine sub10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 60
      if ( dt%d0%a(i) .neqv. .true. ) error stop 62
   end do


end subroutine sub10

subroutine sub10a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 64
      if ( dt%d0%a(i) .neqv. .true. ) error stop 66
   end do


end subroutine sub10a

subroutine sub11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 68
      if ( dt%d1%a(i) .neqv. .true. ) error stop 70
      if ( dt%d1%d0%a(i) .neqv. .true. ) error stop 72
   end do


end subroutine sub11

subroutine sub11a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 74
      if ( dt%d1%a(i) .neqv. .true. ) error stop 76
      if ( dt%d1%d0%a(i) .neqv. .true. ) error stop 78
   end do


end subroutine sub11a

subroutine sub12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 80
      if ( dt%d1%a(i) .neqv. .true. ) error stop 82
      if ( dt%d1%d0%a(i) .neqv. .true. ) error stop 84
   end do


end subroutine sub12

subroutine sub12a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) .neqv. .true. ) error stop 86
      if ( dt%d1%a(i) .neqv. .true. ) error stop 88
      if ( dt%d1%d0%a(i) .neqv. .true. ) error stop 90
   end do


end subroutine sub12a

subroutine sub13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts0), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = .not. dt%a(i)
   end do


end subroutine sub13

subroutine sub14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts1), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = .not. dt%a(i)
      dt%d0%a(i) = .not. dt%d0%a(i)
   end do


end subroutine sub14

subroutine sub15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19a

   type(dts2), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = .not. dt%a(i)
      dt%d1%a(i) = .not. dt%d1%a(i)
      dt%d1%d0%a(i) = .not. dt%d1%d0%a(i)
   end do


end subroutine sub15
