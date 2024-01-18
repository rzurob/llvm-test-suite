!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisog18b cxisog18b
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
!*  KEYWORD(S)                 : C_INT_FAST8_T, C_INTMAX_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_INT_FAST8_T and C_INTMAX_T
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
      integer(C_INT_FAST8_T) :: a(10,5)
      integer(C_INTMAX_T) :: b(6,3)
   end type

   type, bind(c) :: dtd1
      integer(C_INT_FAST8_T) :: a(10,5)
      integer(C_INTMAX_T) :: b(6,3)
      type(dtd0) :: d0
   end type

   type, bind(c) :: dtd2
      integer(C_INT_FAST8_T) :: a(10,5)
      integer(C_INTMAX_T) :: b(6,3)
      type(dtd1) :: d1
   end type

end module mxisob18b

integer(C_INT_FAST8_T) function fnt1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 20
         dt%a(j,i) = i+j
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 22
         dt%b(j,i) = i+j
      end do
   end do

   fnt1 = 0
end function fnt1

integer(C_INT_FAST8_T) function fnt2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 24
         dt%a(j,i) = i+j
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 26
         dt%b(j,i) = i+j
      end do
   end do

   fnt2 = 0
end function fnt2

integer(C_INT_FAST8_T) function fnt3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 28
         dt%a(j,i) = i+j
         if ( dt%d0%a(j,i) /= i+j-1 ) error stop 30
         dt%d0%a(j,i) = dt%d0%a(j,i)+i+j
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 32
         dt%b(j,i) = i+j
         if ( dt%d0%b(j,i) /= i+j-1 ) error stop 34
         dt%d0%b(j,i) = dt%d0%b(j,i)+i+j
      end do
   end do

   fnt3 = 0
end function fnt3

integer(C_INT_FAST8_T) function fnt4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 36
         dt%a(j,i) = i+j
         if ( dt%d0%a(j,i) /= i+j-1 ) error stop 38
         dt%d0%a(j,i) = dt%d0%a(j,i)+i+j
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 40
         dt%b(j,i) = i+j
         if ( dt%d0%b(j,i) /= i+j-1 ) error stop 42
         dt%d0%b(j,i) = dt%d0%b(j,i)+i+j
      end do
   end do

   fnt4 = 0
end function fnt4

integer(C_INT_FAST8_T) function fnt5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), intent(inout) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 44
         dt%a(j,i) = i+j
         if ( dt%d1%a(j,i) /= i+j-1 ) error stop 46
         dt%d1%a(j,i) = dt%d1%a(j,i)+i+j
         if ( dt%d1%d0%a(j,i) /= i+j-1 ) error stop 48
         dt%d1%d0%a(j,i) = dt%d1%d0%a(j,i)+i+j
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 50
         dt%b(j,i) = i+j
         if ( dt%d1%b(j,i) /= i+j-1 ) error stop 52
         dt%d1%b(j,i) = dt%d1%b(j,i)+i+j
         if ( dt%d1%d0%b(j,i) /= i+j-1 ) error stop 54
         dt%d1%d0%b(j,i) = dt%d1%d0%b(j,i)+i+j
      end do
   end do

   fnt5 = 0
end function fnt5

integer(C_INT_FAST8_T) function fnt6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 56
         dt%a(j,i) = i+j
         if ( dt%d1%a(j,i) /= i+j-1 ) error stop 58
         dt%d1%a(j,i) = dt%d1%a(j,i)+i+j
         if ( dt%d1%d0%a(j,i) /= i+j-1 ) error stop 60
         dt%d1%d0%a(j,i) = dt%d1%d0%a(j,i)+i+j
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 62
         dt%b(j,i) = i+j
         if ( dt%d1%b(j,i) /= i+j-1 ) error stop 64
         dt%d1%b(j,i) = dt%d1%b(j,i)+i+j
         if ( dt%d1%d0%b(j,i) /= i+j-1 ) error stop 66
         dt%d1%d0%b(j,i) = dt%d1%d0%b(j,i)+i+j
      end do
   end do

   fnt6 = 0
end function fnt6

integer(C_INT_FAST8_T) function fnt7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 68
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 70
      end do
   end do

   fnt7 = 0
end function fnt7

integer(C_INT_FAST8_T) function fnt7a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 72
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 74
      end do
   end do

   fnt7a = 0
end function fnt7a

integer(C_INT_FAST8_T) function fnt8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 76
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 78
      end do
   end do

   fnt8 = 0
end function fnt8

integer(C_INT_FAST8_T) function fnt8a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 80
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 82
      end do
   end do

   fnt8a = 0
end function fnt8a

integer(C_INT_FAST8_T) function fnt9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 84
         if ( dt%d0%a(j,i) /= i+j-1 ) error stop 86
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 88
         if ( dt%d0%b(j,i) /= i+j-1 ) error stop 90
      end do
   end do

   fnt9 = 0
end function fnt9

integer(C_INT_FAST8_T) function fnt9a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 92
         if ( dt%d0%a(j,i) /= i+j-1 ) error stop 94
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 96
         if ( dt%d0%b(j,i) /= i+j-1 ) error stop 98
      end do
   end do

   fnt9a = 0
end function fnt9a

integer(C_INT_FAST8_T) function fnt10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 100
         if ( dt%d0%a(j,i) /= i+j-1 ) error stop 102
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 104
         if ( dt%d0%b(j,i) /= i+j-1 ) error stop 106
      end do
   end do

   fnt10 = 0
end function fnt10

integer(C_INT_FAST8_T) function fnt10a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 108
         if ( dt%d0%a(j,i) /= i+j-1 ) error stop 110
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 112
         if ( dt%d0%b(j,i) /= i+j-1 ) error stop 114
      end do
   end do

   fnt10a = 0
end function fnt10a

integer(C_INT_FAST8_T) function fnt11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 116
         if ( dt%d1%a(j,i) /= i+j-1 ) error stop 118
         if ( dt%d1%d0%a(j,i) /= i+j-1 ) error stop 120
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 122
         if ( dt%d1%b(j,i) /= i+j-1 ) error stop 124
         if ( dt%d1%d0%b(j,i) /= i+j-1 ) error stop 126
      end do
   end do

   fnt11 = 0
end function fnt11

integer(C_INT_FAST8_T) function fnt11a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), intent(in) :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 128
         if ( dt%d1%a(j,i) /= i+j-1 ) error stop 130
         if ( dt%d1%d0%a(j,i) /= i+j-1 ) error stop 132
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 134
         if ( dt%d1%b(j,i) /= i+j-1 ) error stop 136
         if ( dt%d1%d0%b(j,i) /= i+j-1 ) error stop 138
      end do
   end do

   fnt11a = 0
end function fnt11a

integer(C_INT_FAST8_T) function fnt12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 140
         if ( dt%d1%a(j,i) /= i+j-1 ) error stop 142
         if ( dt%d1%d0%a(j,i) /= i+j-1 ) error stop 144
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 146
         if ( dt%d1%b(j,i) /= i+j-1 ) error stop 148
         if ( dt%d1%d0%b(j,i) /= i+j-1 ) error stop 150
      end do
   end do

   fnt12 = 0
end function fnt12

integer(C_INT_FAST8_T) function fnt12a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), intent(in), value :: dt

   do i = 1, 5
      do j = 1, 10
         if ( dt%a(j,i) /= i+j-1 ) error stop 152
         if ( dt%d1%a(j,i) /= i+j-1 ) error stop 154
         if ( dt%d1%d0%a(j,i) /= i+j-1 ) error stop 156
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         if ( dt%b(j,i) /= i+j-1 ) error stop 158
         if ( dt%d1%b(j,i) /= i+j-1 ) error stop 160
         if ( dt%d1%d0%b(j,i) /= i+j-1 ) error stop 162
      end do
   end do

   fnt12a = 0
end function fnt12a

integer(C_INT_FAST8_T) function fnt13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd0), intent(out) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = i+j
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         dt%b(j,i) = i+j
      end do
   end do

   fnt13 = 0
end function fnt13

integer(C_INT_FAST8_T) function fnt14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd1), intent(out) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = i+j
         dt%d0%a(j,i) = dt%d0%a(j,i)+i+j
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         dt%b(j,i) = i+j
         dt%d0%b(j,i) = dt%d0%b(j,i)+i+j
      end do
   end do

   fnt14 = 0
end function fnt14

integer(C_INT_FAST8_T) function fnt15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob18b

   type(dtd2), intent(out) :: dt

   do i = 1, 5
      do j = 1, 10
         dt%a(j,i) = i+j
         dt%d1%a(j,i) = dt%d1%a(j,i)+i+j
         dt%d1%d0%a(j,i) = dt%d1%d0%a(j,i)+i+j
      end do
   end do

   do i = 1, 3
      do j = 1, 6
         dt%b(j,i) = i+j
         dt%d1%b(j,i) = dt%d1%b(j,i)+i+j
         dt%d1%d0%b(j,i) = dt%d1%d0%b(j,i)+i+j
      end do
   end do

   fnt15 = 0
end function fnt15
