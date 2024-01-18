!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisop00.presh fxisop19b cxisop19b
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
!*  KEYWORD(S)                 : 16
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*
!*      - testing 16
!*	- using external FORTRAN subroutines
!*	- passing derived types with 2-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890
!
module mxisob19b
   use ISO_C_BINDING

   type, bind(c) :: dtd0
      real(16) :: a(10,5)
   end type

   type, bind(c) :: dtd1
      real(16) :: a(10,5)
      type(dtd0) :: d0
   end type

   type, bind(c) :: dtd2
      real(16) :: a(10,5)
      type(dtd1) :: d1
   end type

end module mxisob19b

subroutine sub1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd0), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 20
         dt%a(j,i) = real(i+j,16)
      end do
   end do


end subroutine sub1

subroutine sub2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd0), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 22
         dt%a(j,i) = real(i+j,16)
      end do
   end do


end subroutine sub2

subroutine sub3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd1), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 24
         dt%a(j,i) = real(i+j,16)
         if ( dt%d0%a(j,i) /= real(i+j-1,16) ) error stop 26
         dt%d0%a(j,i) = real(dt%d0%a(j,i)+i+j,16)
      end do
   end do


end subroutine sub3

subroutine sub4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd1), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 28
         dt%a(j,i) = real(i+j,16)
         if ( dt%d0%a(j,i) /= real(i+j-1,16) ) error stop 30
         dt%d0%a(j,i) = real(dt%d0%a(j,i)+i+j,16)
      end do
   end do


end subroutine sub4

subroutine sub5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd2), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 32
         dt%a(j,i) = real(i+j,16)
         if ( dt%d1%a(j,i) /= real(i+j-1,16) ) error stop 34
         dt%d1%a(j,i) = real(dt%d1%a(j,i)+i+j,16)
         if ( dt%d1%d0%a(j,i) /= real(i+j-1,16) ) error stop 36
         dt%d1%d0%a(j,i) = real(dt%d1%d0%a(j,i)+i+j,16)
      end do
   end do


end subroutine sub5

subroutine sub6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd2), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 38
         dt%a(j,i) = real(i+j,16)
         if ( dt%d1%a(j,i) /= real(i+j-1,16) ) error stop 40
         dt%d1%a(j,i) = real(dt%d1%a(j,i)+i+j,16)
         if ( dt%d1%d0%a(j,i) /= real(i+j-1,16) ) error stop 42
         dt%d1%d0%a(j,i) = real(dt%d1%d0%a(j,i)+i+j,16)
      end do
   end do


end subroutine sub6

subroutine sub7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd0), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 44
      end do
   end do


end subroutine sub7

subroutine sub7a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd0), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 46
      end do
   end do


end subroutine sub7a

subroutine sub8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd0), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 48
      end do
   end do


end subroutine sub8

subroutine sub8a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd0), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 50
      end do
   end do


end subroutine sub8a

subroutine sub9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd1), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 52
         if ( dt%d0%a(j,i) /= real(i+j-1,16) ) error stop 54
      end do
   end do


end subroutine sub9

subroutine sub9a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd1), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 56
         if ( dt%d0%a(j,i) /= real(i+j-1,16) ) error stop 58
      end do
   end do


end subroutine sub9a

subroutine sub10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd1), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 60
         if ( dt%d0%a(j,i) /= real(i+j-1,16) ) error stop 62
      end do
   end do


end subroutine sub10

subroutine sub10a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd1), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 64
         if ( dt%d0%a(j,i) /= real(i+j-1,16) ) error stop 66
      end do
   end do


end subroutine sub10a

subroutine sub11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd2), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 68
         if ( dt%d1%a(j,i) /= real(i+j-1,16) ) error stop 70
         if ( dt%d1%d0%a(j,i) /= real(i+j-1,16) ) error stop 72
      end do
   end do


end subroutine sub11

subroutine sub11a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd2), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 74
         if ( dt%d1%a(j,i) /= real(i+j-1,16) ) error stop 76
         if ( dt%d1%d0%a(j,i) /= real(i+j-1,16) ) error stop 78
      end do
   end do


end subroutine sub11a

subroutine sub12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd2), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 80
         if ( dt%d1%a(j,i) /= real(i+j-1,16) ) error stop 82
         if ( dt%d1%d0%a(j,i) /= real(i+j-1,16) ) error stop 84
      end do
   end do


end subroutine sub12

subroutine sub12a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd2), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,16) ) error stop 86
         if ( dt%d1%a(j,i) /= real(i+j-1,16) ) error stop 88
         if ( dt%d1%d0%a(j,i) /= real(i+j-1,16) ) error stop 90
      end do
   end do


end subroutine sub12a

subroutine sub13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd0), intent(out) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = real(i+j,16)
      end do
   end do


end subroutine sub13

subroutine sub14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd1), intent(out) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = real(i+j,16)
         dt%d0%a(j,i) = real(dt%d0%a(j,i)+i+j,16)
      end do
   end do


end subroutine sub14

subroutine sub15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob19b

   type(dtd2), intent(out) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = real(i+j,16)
         dt%d1%a(j,i) = real(dt%d1%a(j,i)+i+j,16)
         dt%d1%d0%a(j,i) = real(dt%d1%d0%a(j,i)+i+j,16)
      end do
   end do


end subroutine sub15
