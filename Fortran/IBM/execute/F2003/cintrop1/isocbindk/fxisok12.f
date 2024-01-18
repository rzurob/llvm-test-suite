!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisok12 cxisok12
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
!*	- using external FORTRAN functions
!*	- passing derived types with scalar fields as arguments
!*      - testing INTENT and VALUE attributes
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob12
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

end module mxisob12

integer(C_SIGNED_CHAR) function fnt1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(inout) :: dt

   if ( dt%a /= 'A' ) error stop 20
   if ( dt%b /= iachar('B') ) error stop 22

   dt%a = 'C'
   dt%b = iachar('D')

   fnt1 = 0
end function fnt1

integer(C_SIGNED_CHAR) function fnt2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), value :: dt

   if ( dt%a /= 'A' ) error stop 24
   if ( dt%b /= iachar('B') ) error stop 26

   dt%a = 'C'
   dt%b = iachar('D')

   fnt2 = 0
end function fnt2

integer(C_SIGNED_CHAR) function fnt3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(inout) :: dt

   if ( dt%a /= 'A' ) error stop 28
   if ( dt%b /= iachar('B') ) error stop 30
   if ( dt%d0%a /= 'A' ) error stop 32
   if ( dt%d0%b /= iachar('B') ) error stop 34

   dt%a = 'C'
   dt%b = iachar('D')
   dt%d0%a = 'C'
   dt%d0%b = iachar('D')

   fnt3 = 0
end function fnt3

integer(C_SIGNED_CHAR) function fnt4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), value :: dt

   if ( dt%a /= 'A' ) error stop 36
   if ( dt%b /= iachar('B') ) error stop 38
   if ( dt%d0%a /= 'A' ) error stop 40
   if ( dt%d0%b /= iachar('B') ) error stop 42

   dt%a = 'C'
   dt%b = iachar('D')
   dt%d0%a = 'C'
   dt%d0%b = iachar('D')

   fnt4 = 0
end function fnt4

integer(C_SIGNED_CHAR) function fnt5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

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

   fnt5 = 0
end function fnt5

integer(C_SIGNED_CHAR) function fnt6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

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

   fnt6 = 0
end function fnt6

integer(C_SIGNED_CHAR) function fnt7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(in) :: dt

   if ( dt%a /= 'A' ) error stop 68
   if ( dt%b /= iachar('B') ) error stop 70

   fnt7 = 0
end function fnt7

integer(C_SIGNED_CHAR) function fnt7a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(in) :: dt

   if ( dt%a /= 'A' ) error stop 72
   if ( dt%b /= iachar('B') ) error stop 74

   fnt7a = 0
end function fnt7a

integer(C_SIGNED_CHAR) function fnt8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(in), value :: dt

   if ( dt%a /= 'A' ) error stop 76
   if ( dt%b /= iachar('B') ) error stop 78

   fnt8 = 0
end function fnt8

integer(C_SIGNED_CHAR) function fnt8a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(in), value :: dt

   if ( dt%a /= 'A' ) error stop 80
   if ( dt%b /= iachar('B') ) error stop 82

   fnt8a = 0
end function fnt8a

integer(C_SIGNED_CHAR) function fnt9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(in) :: dt

   if ( dt%a /= 'A' ) error stop 84
   if ( dt%b /= iachar('B') ) error stop 86
   if ( dt%d0%a /= 'A' ) error stop 88
   if ( dt%d0%b /= iachar('B') ) error stop 90

   fnt9 = 0
end function fnt9

integer(C_SIGNED_CHAR) function fnt9a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(in) :: dt

   if ( dt%a /= 'A' ) error stop 92
   if ( dt%b /= iachar('B') ) error stop 94
   if ( dt%d0%a /= 'A' ) error stop 96
   if ( dt%d0%b /= iachar('B') ) error stop 98

   fnt9a = 0
end function fnt9a

integer(C_SIGNED_CHAR) function fnt10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(in), value :: dt

   if ( dt%a /= 'A' ) error stop 100
   if ( dt%b /= iachar('B') ) error stop 102
   if ( dt%d0%a /= 'A' ) error stop 104
   if ( dt%d0%b /= iachar('B') ) error stop 106

   fnt10 = 0
end function fnt10

integer(C_SIGNED_CHAR) function fnt10a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(in), value :: dt

   if ( dt%a /= 'A' ) error stop 108
   if ( dt%b /= iachar('B') ) error stop 110
   if ( dt%d0%a /= 'A' ) error stop 112
   if ( dt%d0%b /= iachar('B') ) error stop 114

   fnt10a = 0
end function fnt10a

integer(C_SIGNED_CHAR) function fnt11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(in) :: dt

   if ( dt%a /= 'A' ) error stop 116
   if ( dt%b /= iachar('B') ) error stop 118
   if ( dt%d1%a /= 'A' ) error stop 120
   if ( dt%d1%b /= iachar('B') ) error stop 122
   if ( dt%d1%d0%a /= 'A' ) error stop 124
   if ( dt%d1%d0%b /= iachar('B') ) error stop 126

   fnt11 = 0
end function fnt11

integer(C_SIGNED_CHAR) function fnt11a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(in) :: dt

   if ( dt%a /= 'A' ) error stop 128
   if ( dt%b /= iachar('B') ) error stop 130
   if ( dt%d1%a /= 'A' ) error stop 132
   if ( dt%d1%b /= iachar('B') ) error stop 134
   if ( dt%d1%d0%a /= 'A' ) error stop 136
   if ( dt%d1%d0%b /= iachar('B') ) error stop 138

   fnt11a = 0
end function fnt11a

integer(C_SIGNED_CHAR) function fnt12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(in), value :: dt

   if ( dt%a /= 'A' ) error stop 140
   if ( dt%b /= iachar('B') ) error stop 142
   if ( dt%d1%a /= 'A' ) error stop 144
   if ( dt%d1%b /= iachar('B') ) error stop 146
   if ( dt%d1%d0%a /= 'A' ) error stop 148
   if ( dt%d1%d0%b /= iachar('B') ) error stop 150

   fnt12 = 0
end function fnt12

integer(C_SIGNED_CHAR) function fnt12a(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(in), value :: dt

   if ( dt%a /= 'A' ) error stop 152
   if ( dt%b /= iachar('B') ) error stop 154
   if ( dt%d1%a /= 'A' ) error stop 156
   if ( dt%d1%b /= iachar('B') ) error stop 158
   if ( dt%d1%d0%a /= 'A' ) error stop 160
   if ( dt%d1%d0%b /= iachar('B') ) error stop 162

   fnt12a = 0
end function fnt12a

integer(C_SIGNED_CHAR) function fnt13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt0), intent(out) :: dt

   dt%a = 'C'
   dt%b = iachar('D')

   fnt13 = 0
end function fnt13

integer(C_SIGNED_CHAR) function fnt14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt1), intent(out) :: dt

   dt%a = 'C'
   dt%b = iachar('D')
   dt%d0%a = 'C'
   dt%d0%b = iachar('D')

   fnt14 = 0
end function fnt14

integer(C_SIGNED_CHAR) function fnt15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob12

   type(dt2), intent(out) :: dt

   dt%a = 'C'
   dt%b = iachar('D')
   dt%d1%a = 'C'
   dt%d1%b = iachar('D')
   dt%d1%d0%a = 'C'
   dt%d1%d0%b = iachar('D')

   fnt15 = 0
end function fnt15

