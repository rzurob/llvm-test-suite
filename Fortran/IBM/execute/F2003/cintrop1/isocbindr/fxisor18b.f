!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisor00.presh fxisor18b cxisor18b
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
!*      - testing C_BOOL
!*	- using external FORTRAN functions
!*	- passing derived types with 2-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890
!
module mxisob18b
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

end module mxisob18b

logical(C_BOOL) function fnt1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 20
         dt%a(j,i) = .not. dt%a(j,i)
      end do
   end do


   fnt1 = .false.
end function fnt1

logical(C_BOOL) function fnt2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 22
         dt%a(j,i) = .not. dt%a(j,i)
      end do
   end do


   fnt2 = .false.
end function fnt2

logical(C_BOOL) function fnt3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 24
         dt%a(j,i) = .not. dt%a(j,i)
         if ( dt%d0%a(j,i) .neqv. .true. ) error stop 26
         dt%d0%a(j,i) = .not. dt%d0%a(j,i)
      end do
   end do


   fnt3 = .false.
end function fnt3

logical(C_BOOL) function fnt4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 28
         dt%a(j,i) = .not. dt%a(j,i)
         if ( dt%d0%a(j,i) .neqv. .true. ) error stop 30
         dt%d0%a(j,i) = .not. dt%d0%a(j,i)
      end do
   end do


   fnt4 = .false.
end function fnt4

logical(C_BOOL) function fnt5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 32
         dt%a(j,i) = .not. dt%a(j,i)
         if ( dt%d1%a(j,i) .neqv. .true. ) error stop 34
         dt%d1%a(j,i) = .not. dt%d1%a(j,i)
         if ( dt%d1%d0%a(j,i) .neqv. .true. ) error stop 36
         dt%d1%d0%a(j,i) = .not. dt%d1%d0%a(j,i)
      end do
   end do


   fnt5 = .false.
end function fnt5

logical(C_BOOL) function fnt6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 38
         dt%a(j,i) = .not. dt%a(j,i)
         if ( dt%d1%a(j,i) .neqv. .true. ) error stop 40
         dt%d1%a(j,i) = .not. dt%d1%a(j,i)
         if ( dt%d1%d0%a(j,i) .neqv. .true. ) error stop 42
         dt%d1%d0%a(j,i) = .not. dt%d1%d0%a(j,i)
      end do
   end do


   fnt6 = .false.
end function fnt6

logical(C_BOOL) function fnt7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 44
      end do
   end do


   fnt7 = .false.
end function fnt7

logical(C_BOOL) function fnt7a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 46
      end do
   end do


   fnt7a = .false.
end function fnt7a

logical(C_BOOL) function fnt8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 48
      end do
   end do


   fnt8 = .false.
end function fnt8

logical(C_BOOL) function fnt8a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 50
      end do
   end do


   fnt8a = .false.
end function fnt8a

logical(C_BOOL) function fnt9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 52
         if ( dt%d0%a(j,i) .neqv. .true. ) error stop 54
      end do
   end do


   fnt9 = .false.
end function fnt9

logical(C_BOOL) function fnt9a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 56
         if ( dt%d0%a(j,i) .neqv. .true. ) error stop 58
      end do
   end do


   fnt9a = .false.
end function fnt9a

logical(C_BOOL) function fnt10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 60
         if ( dt%d0%a(j,i) .neqv. .true. ) error stop 62
      end do
   end do


   fnt10 = .false.
end function fnt10

logical(C_BOOL) function fnt10a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 64
         if ( dt%d0%a(j,i) .neqv. .true. ) error stop 66
      end do
   end do


   fnt10a = .false.
end function fnt10a

logical(C_BOOL) function fnt11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 68
         if ( dt%d1%a(j,i) .neqv. .true. ) error stop 70
         if ( dt%d1%d0%a(j,i) .neqv. .true. ) error stop 72
      end do
   end do


   fnt11 = .false.
end function fnt11

logical(C_BOOL) function fnt11a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 74
         if ( dt%d1%a(j,i) .neqv. .true. ) error stop 76
         if ( dt%d1%d0%a(j,i) .neqv. .true. ) error stop 78
      end do
   end do


   fnt11a = .false.
end function fnt11a

logical(C_BOOL) function fnt12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 80
         if ( dt%d1%a(j,i) .neqv. .true. ) error stop 82
         if ( dt%d1%d0%a(j,i) .neqv. .true. ) error stop 84
      end do
   end do


   fnt12 = .false.
end function fnt12

logical(C_BOOL) function fnt12a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) .neqv. .true. ) error stop 86
         if ( dt%d1%a(j,i) .neqv. .true. ) error stop 88
         if ( dt%d1%d0%a(j,i) .neqv. .true. ) error stop 90
      end do
   end do


   fnt12a = .false.
end function fnt12a

logical(C_BOOL) function fnt13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

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
   use mxisob18b

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
   use mxisob18b

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
