!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisok15 cxisok14
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
!*  KEYWORD(S)                 : C_CHAR, C_SIGNED_CHAR
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_CHAR and C_SIGNED_CHAR
!*      - using C functions with interfaces to FORTRAN functions
!*      - function interfaces defined in module
!*      - passing derived types with scalar fields as arguments
!*      - testing INTENT and VALUE attributes
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob15
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

end module mxisob15

module mxisob15a
   interface
      integer(C_SIGNED_CHAR) function fnt1(dt) bind(c)
         use mxisob15
         type(dt0), intent(inout) :: dt
      end function fnt1
      integer(C_SIGNED_CHAR) function fnt2(dt) bind(c)
         use mxisob15
         type(dt0), value :: dt
      end function fnt2
      integer(C_SIGNED_CHAR) function fnt3(dt) bind(c)
         use mxisob15
         type(dt1), intent(inout) :: dt
      end function fnt3
      integer(C_SIGNED_CHAR) function fnt4(dt) bind(c)
         use mxisob15
         type(dt1), value :: dt
      end function fnt4
      integer(C_SIGNED_CHAR) function fnt5(dt) bind(c)
         use mxisob15
         type(dt2), intent(inout) :: dt
      end function fnt5
      integer(C_SIGNED_CHAR) function fnt6(dt) bind(c)
         use mxisob15
         type(dt2), value :: dt
      end function fnt6
      integer(C_SIGNED_CHAR) function fnt7(dt) bind(c)
         use mxisob15
         type(dt0), intent(in) :: dt
      end function fnt7
      integer(C_SIGNED_CHAR) function fnt7a(dt) bind(c)
         use mxisob15
         type(dt0), intent(in) :: dt
      end function fnt7a
      integer(C_SIGNED_CHAR) function fnt8(dt) bind(c)
         use mxisob15
         type(dt0), intent(in), value :: dt
      end function fnt8
      integer(C_SIGNED_CHAR) function fnt8a(dt) bind(c)
         use mxisob15
         type(dt0), intent(in), value :: dt
      end function fnt8a
      integer(C_SIGNED_CHAR) function fnt9(dt) bind(c)
         use mxisob15
         type(dt1), intent(in) :: dt
      end function fnt9
      integer(C_SIGNED_CHAR) function fnt9a(dt) bind(c)
         use mxisob15
         type(dt1), intent(in) :: dt
      end function fnt9a
      integer(C_SIGNED_CHAR) function fnt10(dt) bind(c)
         use mxisob15
         type(dt1), intent(in), value :: dt
      end function fnt10
      integer(C_SIGNED_CHAR) function fnt10a(dt) bind(c)
         use mxisob15
         type(dt1), intent(in), value :: dt
      end function fnt10a
      integer(C_SIGNED_CHAR) function fnt11(dt) bind(c)
         use mxisob15
         type(dt2), intent(in) :: dt
      end function fnt11
      integer(C_SIGNED_CHAR) function fnt11a(dt) bind(c)
         use mxisob15
         type(dt2), intent(in) :: dt
      end function fnt11a
      integer(C_SIGNED_CHAR) function fnt12(dt) bind(c)
         use mxisob15
         type(dt2), intent(in), value :: dt
      end function fnt12
      integer(C_SIGNED_CHAR) function fnt12a(dt) bind(c)
         use mxisob15
         type(dt2), intent(in), value :: dt
      end function fnt12a
      integer(C_SIGNED_CHAR) function fnt13(dt) bind(c)
         use mxisob15
         type(dt0), intent(out) :: dt
      end function fnt13
      integer(C_SIGNED_CHAR) function fnt14(dt) bind(c)
         use mxisob15
         type(dt1), intent(out) :: dt
      end function fnt14
      integer(C_SIGNED_CHAR) function fnt15(dt) bind(c)
         use mxisob15
         type(dt2), intent(out) :: dt
      end function fnt15
   end interface
end module mxisob15a

program fxisok15
   use ISO_C_BINDING
   use mxisob15
   use mxisob15a

   type(dt0) :: dta
   type(dt1) :: dtb
   type(dt2) :: dtc
   integer ret

!! Test 1

   dta%a = 'A'
   dta%b = iachar('B')

   ret = fnt1(dta)

   if ( dta%a /= 'C' ) error stop 20
   if ( dta%b /= iachar('D') ) error stop 22

!! Test 2

   dta%a = 'A'
   dta%b = iachar('B')

   ret = fnt2(dta)

   if ( dta%a /= 'A' ) error stop 24
   if ( dta%b /= iachar('B') ) error stop 26

!! Test 3

   dtb%a = 'A'
   dtb%b = iachar('B')
   dtb%d0%a = 'A'
   dtb%d0%b = iachar('B')

   ret = fnt3(dtb)

   if ( dtb%a /= 'C' ) error stop 28
   if ( dtb%b /= iachar('D') ) error stop 30
   if ( dtb%d0%a /= 'C' ) error stop 32
   if ( dtb%d0%b /= iachar('D') ) error stop 34

!! Test 4

   dtb%a = 'A'
   dtb%b = iachar('B')
   dtb%d0%a = 'A'
   dtb%d0%b = iachar('B')

   ret = fnt4(dtb)

   if ( dtb%a /= 'A' ) error stop 36
   if ( dtb%b /= iachar('B') ) error stop 38
   if ( dtb%d0%a /= 'A' ) error stop 40
   if ( dtb%d0%b /= iachar('B') ) error stop 42

!! Test 5

   dtc%a = 'A'
   dtc%b = iachar('B')
   dtc%d1%a = 'A'
   dtc%d1%b = iachar('B')
   dtc%d1%d0%a = 'A'
   dtc%d1%d0%b = iachar('B')

   ret = fnt5(dtc)

   if ( dtc%a /= 'C' ) error stop 44
   if ( dtc%b /= iachar('D') ) error stop 46
   if ( dtc%d1%a /= 'C' ) error stop 48
   if ( dtc%d1%b /= iachar('D') ) error stop 50
   if ( dtc%d1%d0%a /= 'C' ) error stop 52
   if ( dtc%d1%d0%b /= iachar('D') ) error stop 54

!! Test 6

   dtc%a = 'A'
   dtc%b = iachar('B')
   dtc%d1%a = 'A'
   dtc%d1%b = iachar('B')
   dtc%d1%d0%a = 'A'
   dtc%d1%d0%b = iachar('B')

   ret = fnt6(dtc)

   if ( dtc%a /= 'A' ) error stop 56
   if ( dtc%b /= iachar('B') ) error stop 58
   if ( dtc%d1%a /= 'A' ) error stop 60
   if ( dtc%d1%b /= iachar('B') ) error stop 62
   if ( dtc%d1%d0%a /= 'A' ) error stop 64
   if ( dtc%d1%d0%b /= iachar('B') ) error stop 66

!! Test 7

   dta%a = 'A'
   dta%b = iachar('B')

   ret = fnt7(dta)

   if ( dta%a /= 'A' ) error stop 68
   if ( dta%b /= iachar('B') ) error stop 70

!! Test 7a

   dta%a = 'A'
   dta%b = iachar('B')

   ret = fnt7a(dta)

   if ( dta%a /= 'A' ) error stop 72
   if ( dta%b /= iachar('B') ) error stop 74

!! Test 8

   dta%a = 'A'
   dta%b = iachar('B')

   ret = fnt8(dta)

   if ( dta%a /= 'A' ) error stop 76
   if ( dta%b /= iachar('B') ) error stop 78

!! Test 8a

   dta%a = 'A'
   dta%b = iachar('B')

   ret = fnt8a(dta)

   if ( dta%a /= 'A' ) error stop 80
   if ( dta%b /= iachar('B') ) error stop 82

!! Test 9

   dtb%a = 'A'
   dtb%b = iachar('B')
   dtb%d0%a = 'A'
   dtb%d0%b = iachar('B')

   ret = fnt9(dtb)

   if ( dtb%a /= 'A' ) error stop 84
   if ( dtb%b /= iachar('B') ) error stop 86
   if ( dtb%d0%a /= 'A' ) error stop 88
   if ( dtb%d0%b /= iachar('B') ) error stop 90

!! Test 9a

   dtb%a = 'A'
   dtb%b = iachar('B')
   dtb%d0%a = 'A'
   dtb%d0%b = iachar('B')

   ret = fnt9a(dtb)

   if ( dtb%a /= 'A' ) error stop 92
   if ( dtb%b /= iachar('B') ) error stop 94
   if ( dtb%d0%a /= 'A' ) error stop 96
   if ( dtb%d0%b /= iachar('B') ) error stop 98

!! Test 10

   dtb%a = 'A'
   dtb%b = iachar('B')
   dtb%d0%a = 'A'
   dtb%d0%b = iachar('B')

   ret = fnt10(dtb)

   if ( dtb%a /= 'A' ) error stop 100
   if ( dtb%b /= iachar('B') ) error stop 102
   if ( dtb%d0%a /= 'A' ) error stop 104
   if ( dtb%d0%b /= iachar('B') ) error stop 106

!! Test 10a

   dtb%a = 'A'
   dtb%b = iachar('B')
   dtb%d0%a = 'A'
   dtb%d0%b = iachar('B')

   ret = fnt10a(dtb)

   if ( dtb%a /= 'A' ) error stop 108
   if ( dtb%b /= iachar('B') ) error stop 110
   if ( dtb%d0%a /= 'A' ) error stop 112
   if ( dtb%d0%b /= iachar('B') ) error stop 114

!! Test 11

   dtc%a = 'A'
   dtc%b = iachar('B')
   dtc%d1%a = 'A'
   dtc%d1%b = iachar('B')
   dtc%d1%d0%a = 'A'
   dtc%d1%d0%b = iachar('B')

   ret = fnt11(dtc)

   if ( dtc%a /= 'A' ) error stop 116
   if ( dtc%b /= iachar('B') ) error stop 118
   if ( dtc%d1%a /= 'A' ) error stop 120
   if ( dtc%d1%b /= iachar('B') ) error stop 122
   if ( dtc%d1%d0%a /= 'A' ) error stop 124
   if ( dtc%d1%d0%b /= iachar('B') ) error stop 126

!! Test 11a

   dtc%a = 'A'
   dtc%b = iachar('B')
   dtc%d1%a = 'A'
   dtc%d1%b = iachar('B')
   dtc%d1%d0%a = 'A'
   dtc%d1%d0%b = iachar('B')

   ret = fnt11a(dtc)

   if ( dtc%a /= 'A' ) error stop 128
   if ( dtc%b /= iachar('B') ) error stop 130
   if ( dtc%d1%a /= 'A' ) error stop 132
   if ( dtc%d1%b /= iachar('B') ) error stop 134
   if ( dtc%d1%d0%a /= 'A' ) error stop 136
   if ( dtc%d1%d0%b /= iachar('B') ) error stop 138

!! Test 12

   dtc%a = 'A'
   dtc%b = iachar('B')
   dtc%d1%a = 'A'
   dtc%d1%b = iachar('B')
   dtc%d1%d0%a = 'A'
   dtc%d1%d0%b = iachar('B')

   ret = fnt12(dtc)

   if ( dtc%a /= 'A' ) error stop 140
   if ( dtc%b /= iachar('B') ) error stop 142
   if ( dtc%d1%a /= 'A' ) error stop 144
   if ( dtc%d1%b /= iachar('B') ) error stop 146
   if ( dtc%d1%d0%a /= 'A' ) error stop 148
   if ( dtc%d1%d0%b /= iachar('B') ) error stop 150

!! Test 12a

   dtc%a = 'A'
   dtc%b = iachar('B')
   dtc%d1%a = 'A'
   dtc%d1%b = iachar('B')
   dtc%d1%d0%a = 'A'
   dtc%d1%d0%b = iachar('B')

   ret = fnt12a(dtc)

   if ( dtc%a /= 'A' ) error stop 152
   if ( dtc%b /= iachar('B') ) error stop 154
   if ( dtc%d1%a /= 'A' ) error stop 156
   if ( dtc%d1%b /= iachar('B') ) error stop 158
   if ( dtc%d1%d0%a /= 'A' ) error stop 160
   if ( dtc%d1%d0%b /= iachar('B') ) error stop 162

!! Test 13

   dta%a = 'A'
   dta%b = iachar('B')

   ret = fnt13(dta)

   if ( dta%a /= 'C' ) error stop 164
   if ( dta%b /= iachar('D') ) error stop 166

!! Test 14

   dtb%a = 'A'
   dtb%b = iachar('B')
   dtb%d0%a = 'A'
   dtb%d0%b = iachar('B')

   ret = fnt14(dtb)

   if ( dtb%a /= 'C' ) error stop 168
   if ( dtb%b /= iachar('D') ) error stop 170
   if ( dtb%d0%a /= 'C' ) error stop 172
   if ( dtb%d0%b /= iachar('D') ) error stop 174

!! Test 15

   dtc%a = 'A'
   dtc%b = iachar('B')
   dtc%d1%a = 'A'
   dtc%d1%b = iachar('B')
   dtc%d1%d0%a = 'A'
   dtc%d1%d0%b = iachar('B')

   ret = fnt15(dtc)

   if ( dtc%a /= 'C' ) error stop 176
   if ( dtc%b /= iachar('D') ) error stop 178
   if ( dtc%d1%a /= 'C' ) error stop 180
   if ( dtc%d1%b /= iachar('D') ) error stop 182
   if ( dtc%d1%d0%a /= 'C' ) error stop 184
   if ( dtc%d1%d0%b /= iachar('D') ) error stop 186

end
