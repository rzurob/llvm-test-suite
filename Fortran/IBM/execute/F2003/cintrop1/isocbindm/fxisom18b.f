!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisom18b cxisom18b
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
!*  KEYWORD(S)                 : C_LONG_DOUBLE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_LONG_DOUBLE
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
      real(C_LONG_DOUBLE) :: a(10,5)
   end type

   type, bind(c) :: dtd1
      real(C_LONG_DOUBLE) :: a(10,5)
      type(dtd0) :: d0
   end type

   type, bind(c) :: dtd2
      real(C_LONG_DOUBLE) :: a(10,5)
      type(dtd1) :: d1
   end type

end module mxisob18b

real(C_LONG_DOUBLE) function fnt1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 20
         dt%a(j,i) = real(i+j,C_LONG_DOUBLE)
      end do
   end do


   fnt1 = 0
end function fnt1

real(C_LONG_DOUBLE) function fnt2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 22
         dt%a(j,i) = real(i+j,C_LONG_DOUBLE)
      end do
   end do


   fnt2 = 0
end function fnt2

real(C_LONG_DOUBLE) function fnt3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 24
         dt%a(j,i) = real(i+j,C_LONG_DOUBLE)
         if ( dt%d0%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 26
         dt%d0%a(j,i) = real(dt%d0%a(j,i)+i+j,C_LONG_DOUBLE)
      end do
   end do


   fnt3 = 0
end function fnt3

real(C_LONG_DOUBLE) function fnt4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 28
         dt%a(j,i) = real(i+j,C_LONG_DOUBLE)
         if ( dt%d0%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 30
         dt%d0%a(j,i) = real(dt%d0%a(j,i)+i+j,C_LONG_DOUBLE)
      end do
   end do


   fnt4 = 0
end function fnt4

real(C_LONG_DOUBLE) function fnt5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 32
         dt%a(j,i) = real(i+j,C_LONG_DOUBLE)
         if ( dt%d1%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 34
         dt%d1%a(j,i) = real(dt%d1%a(j,i)+i+j,C_LONG_DOUBLE)
         if ( dt%d1%d0%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 36
         dt%d1%d0%a(j,i) = real(dt%d1%d0%a(j,i)+i+j,C_LONG_DOUBLE)
      end do
   end do


   fnt5 = 0
end function fnt5

real(C_LONG_DOUBLE) function fnt6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 38
         dt%a(j,i) = real(i+j,C_LONG_DOUBLE)
         if ( dt%d1%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 40
         dt%d1%a(j,i) = real(dt%d1%a(j,i)+i+j,C_LONG_DOUBLE)
         if ( dt%d1%d0%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 42
         dt%d1%d0%a(j,i) = real(dt%d1%d0%a(j,i)+i+j,C_LONG_DOUBLE)
      end do
   end do


   fnt6 = 0
end function fnt6

real(C_LONG_DOUBLE) function fnt7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 44
      end do
   end do


   fnt7 = 0
end function fnt7

real(C_LONG_DOUBLE) function fnt7a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 46
      end do
   end do


   fnt7a = 0
end function fnt7a

real(C_LONG_DOUBLE) function fnt8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 48
      end do
   end do


   fnt8 = 0
end function fnt8

real(C_LONG_DOUBLE) function fnt8a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 50
      end do
   end do


   fnt8a = 0
end function fnt8a

real(C_LONG_DOUBLE) function fnt9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 52
         if ( dt%d0%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 54
      end do
   end do


   fnt9 = 0
end function fnt9

real(C_LONG_DOUBLE) function fnt9a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 56
         if ( dt%d0%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 58
      end do
   end do


   fnt9a = 0
end function fnt9a

real(C_LONG_DOUBLE) function fnt10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 60
         if ( dt%d0%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 62
      end do
   end do


   fnt10 = 0
end function fnt10

real(C_LONG_DOUBLE) function fnt10a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 64
         if ( dt%d0%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 66
      end do
   end do


   fnt10a = 0
end function fnt10a

real(C_LONG_DOUBLE) function fnt11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 68
         if ( dt%d1%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 70
         if ( dt%d1%d0%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 72
      end do
   end do


   fnt11 = 0
end function fnt11

real(C_LONG_DOUBLE) function fnt11a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 74
         if ( dt%d1%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 76
         if ( dt%d1%d0%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 78
      end do
   end do


   fnt11a = 0
end function fnt11a

real(C_LONG_DOUBLE) function fnt12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 80
         if ( dt%d1%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 82
         if ( dt%d1%d0%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 84
      end do
   end do


   fnt12 = 0
end function fnt12

real(C_LONG_DOUBLE) function fnt12a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 86
         if ( dt%d1%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 88
         if ( dt%d1%d0%a(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 90
      end do
   end do


   fnt12a = 0
end function fnt12a

real(C_LONG_DOUBLE) function fnt13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), intent(out) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = real(i+j,C_LONG_DOUBLE)
      end do
   end do


   fnt13 = 0
end function fnt13

real(C_LONG_DOUBLE) function fnt14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), intent(out) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = real(i+j,C_LONG_DOUBLE)
         dt%d0%a(j,i) = real(dt%d0%a(j,i)+i+j,C_LONG_DOUBLE)
      end do
   end do


   fnt14 = 0
end function fnt14

real(C_LONG_DOUBLE) function fnt15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), intent(out) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = real(i+j,C_LONG_DOUBLE)
         dt%d1%a(j,i) = real(dt%d1%a(j,i)+i+j,C_LONG_DOUBLE)
         dt%d1%d0%a(j,i) = real(dt%d1%d0%a(j,i)+i+j,C_LONG_DOUBLE)
      end do
   end do


   fnt15 = 0
end function fnt15
