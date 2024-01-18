!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisok13 cxisok13
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
!*  KEYWORD(S)                 : C_CHAR, C_SIGNED_CHAR
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_CHAR and C_SIGNED_CHAR
!*	- using external fortran subroutines
!*	- passing derived types with scalar fields as arguments
!*      - testing INTENT and VALUE attributes
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob13
   use ISO_C_BINDING

   type, bind(c) :: dt0
      character(C_CHAR) :: a
      integer(C_SIGNED_CHAR) :: b
   end type

   type, bind(c) :: dt1
      character(C_CHAR) :: a
      integer(C_SIGNED_CHAR) :: b
      type(dt0) :: d0
   end type

   type, bind(c) :: dt2
      character(C_CHAR) :: a
      integer(C_SIGNED_CHAR) :: b
      type(dt1) :: d1
   end type

end module mxisob13

subroutine sub1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt0), intent(inout) :: dt

   if ( dt%a /= 'A' ) error stop 20
   if ( dt%b /= iachar('B') ) error stop 22

   dt%a = 'C'
   dt%b = iachar('D')

end subroutine sub1

subroutine sub2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt0), value :: dt

   if ( dt%a /= 'A' ) error stop 24
   if ( dt%b /= iachar('B') ) error stop 26

   dt%a = 'C'
   dt%b = iachar('D')

end subroutine sub2

subroutine sub3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt1), intent(inout) :: dt

   if ( dt%a /= 'A' ) error stop 28
   if ( dt%b /= iachar('B') ) error stop 30
   if ( dt%d0%a /= 'A' ) error stop 32
   if ( dt%d0%b /= iachar('B') ) error stop 34

   dt%a = 'C'
   dt%b = iachar('D')
   dt%d0%a = 'C'
   dt%d0%b = iachar('D')

end subroutine sub3

subroutine sub4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt1), value :: dt

   if ( dt%a /= 'A' ) error stop 36
   if ( dt%b /= iachar('B') ) error stop 38
   if ( dt%d0%a /= 'A' ) error stop 40
   if ( dt%d0%b /= iachar('B') ) error stop 42

   dt%a = 'C'
   dt%b = iachar('D')
   dt%d0%a = 'C'
   dt%d0%b = iachar('D')

end subroutine sub4

subroutine sub5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt2), intent(inout) :: dt

   if ( dt%a /= 'A' ) error stop 44
   if ( dt%b /= iachar('B') ) error stop 46
   if ( dt%d1%a /= 'A' ) error stop 48
   if ( dt%d1%b /= iachar('B') ) error stop 50
   if ( dt%d1%d0%a /= 'A' ) error stop 52
   if ( dt%d1%d0%b /= iachar('B') ) error stop 54

   dt%a = 'C'
   dt%b = iachar('D')
   dt%d1%a = 'C'
   dt%d1%b = iachar('D')
   dt%d1%d0%a = 'C'
   dt%d1%d0%b = iachar('D')

end subroutine sub5

subroutine sub6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt2), value :: dt

   if ( dt%a /= 'A' ) error stop 56
   if ( dt%b /= iachar('B') ) error stop 58
   if ( dt%d1%a /= 'A' ) error stop 60
   if ( dt%d1%b /= iachar('B') ) error stop 62
   if ( dt%d1%d0%a /= 'A' ) error stop 64
   if ( dt%d1%d0%b /= iachar('B') ) error stop 66

   dt%a = 'C'
   dt%b = iachar('D')
   dt%d1%a = 'C'
   dt%d1%b = iachar('D')
   dt%d1%d0%a = 'C'
   dt%d1%d0%b = iachar('D')

end subroutine sub6

subroutine sub7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt0), intent(in) :: dt

   if ( dt%a /= 'A' ) error stop 68
   if ( dt%b /= iachar('B') ) error stop 70

end subroutine sub7

subroutine sub7a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt0), intent(in) :: dt

   if ( dt%a /= 'A' ) error stop 72
   if ( dt%b /= iachar('B') ) error stop 74

end subroutine sub7a

subroutine sub8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt0), intent(in), value :: dt

   if ( dt%a /= 'A' ) error stop 76
   if ( dt%b /= iachar('B') ) error stop 78

end subroutine sub8

subroutine sub8a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt0), intent(in), value :: dt

   if ( dt%a /= 'A' ) error stop 80
   if ( dt%b /= iachar('B') ) error stop 82

end subroutine sub8a

subroutine sub9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt1), intent(in) :: dt

   if ( dt%a /= 'A' ) error stop 84
   if ( dt%b /= iachar('B') ) error stop 86
   if ( dt%d0%a /= 'A' ) error stop 88
   if ( dt%d0%b /= iachar('B') ) error stop 90

end subroutine sub9

subroutine sub9a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt1), intent(in) :: dt

   if ( dt%a /= 'A' ) error stop 92
   if ( dt%b /= iachar('B') ) error stop 94
   if ( dt%d0%a /= 'A' ) error stop 96
   if ( dt%d0%b /= iachar('B') ) error stop 98

end subroutine sub9a

subroutine sub10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt1), intent(in), value :: dt

   if ( dt%a /= 'A' ) error stop 100
   if ( dt%b /= iachar('B') ) error stop 102
   if ( dt%d0%a /= 'A' ) error stop 104
   if ( dt%d0%b /= iachar('B') ) error stop 106

end subroutine sub10

subroutine sub10a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt1), intent(in), value :: dt

   if ( dt%a /= 'A' ) error stop 108
   if ( dt%b /= iachar('B') ) error stop 110
   if ( dt%d0%a /= 'A' ) error stop 112
   if ( dt%d0%b /= iachar('B') ) error stop 114

end subroutine sub10a

subroutine sub11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt2), intent(in) :: dt

   if ( dt%a /= 'A' ) error stop 116
   if ( dt%b /= iachar('B') ) error stop 118
   if ( dt%d1%a /= 'A' ) error stop 120
   if ( dt%d1%b /= iachar('B') ) error stop 122
   if ( dt%d1%d0%a /= 'A' ) error stop 124
   if ( dt%d1%d0%b /= iachar('B') ) error stop 126

end subroutine sub11

subroutine sub11a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt2), intent(in) :: dt

   if ( dt%a /= 'A' ) error stop 128
   if ( dt%b /= iachar('B') ) error stop 130
   if ( dt%d1%a /= 'A' ) error stop 132
   if ( dt%d1%b /= iachar('B') ) error stop 134
   if ( dt%d1%d0%a /= 'A' ) error stop 136
   if ( dt%d1%d0%b /= iachar('B') ) error stop 138

end subroutine sub11a

subroutine sub12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt2), intent(in), value :: dt

   if ( dt%a /= 'A' ) error stop 140
   if ( dt%b /= iachar('B') ) error stop 142
   if ( dt%d1%a /= 'A' ) error stop 144
   if ( dt%d1%b /= iachar('B') ) error stop 146
   if ( dt%d1%d0%a /= 'A' ) error stop 148
   if ( dt%d1%d0%b /= iachar('B') ) error stop 150

end subroutine sub12

subroutine sub12a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt2), intent(in), value :: dt

   if ( dt%a /= 'A' ) error stop 152
   if ( dt%b /= iachar('B') ) error stop 154
   if ( dt%d1%a /= 'A' ) error stop 156
   if ( dt%d1%b /= iachar('B') ) error stop 158
   if ( dt%d1%d0%a /= 'A' ) error stop 160
   if ( dt%d1%d0%b /= iachar('B') ) error stop 162

end subroutine sub12a

subroutine sub13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt0), intent(out) :: dt

   dt%a = 'C'
   dt%b = iachar('D')

end subroutine sub13

subroutine sub14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt1), intent(out) :: dt

   dt%a = 'C'
   dt%b = iachar('D')
   dt%d0%a = 'C'
   dt%d0%b = iachar('D')

end subroutine sub14

subroutine sub15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob13

   type(dt2), intent(out) :: dt

   dt%a = 'C'
   dt%b = iachar('D')
   dt%d1%a = 'C'
   dt%d1%b = iachar('D')
   dt%d1%d0%a = 'C'
   dt%d1%d0%b = iachar('D')

end subroutine sub15

