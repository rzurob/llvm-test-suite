!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa01.presh fxison27a
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
!*  KEYWORD(S)                 : C_FLOAT_COMPLEX, C_DOUBLE_COMPLEX
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_FLOAT_COMPLEX and C_DOUBLE_COMPLEX
!*      - FORTRAN code only
!*      - passing derived types with 1-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob27a
   use ISO_C_BINDING

   type, bind(c) :: dts0
      complex(C_FLOAT_COMPLEX) :: a(5)
      complex(C_DOUBLE_COMPLEX) :: b(3)
   end type

   type, bind(c) :: dts1
      complex(C_FLOAT_COMPLEX) :: a(5)
      complex(C_DOUBLE_COMPLEX) :: b(3)
      type(dts0) :: d0
   end type

   type, bind(c) :: dts2
      complex(C_FLOAT_COMPLEX) :: a(5)
      complex(C_DOUBLE_COMPLEX) :: b(3)
      type(dts1) :: d1
   end type

end module mxisob27a

program fxison27a
   use ISO_C_BINDING
   use mxisob27a

   interface
      complex(C_FLOAT_COMPLEX) function fnt1(dt) bind(c)
         use mxisob27a
         type(dts0), intent(inout) :: dt
      end function fnt1
      complex(C_FLOAT_COMPLEX) function fnt2(dt) bind(c)
         use mxisob27a
         type(dts0), value :: dt
      end function fnt2
      complex(C_FLOAT_COMPLEX) function fnt3(dt) bind(c)
         use mxisob27a
         type(dts1), intent(inout) :: dt
      end function fnt3
      complex(C_FLOAT_COMPLEX) function fnt4(dt) bind(c)
         use mxisob27a
         type(dts1), value :: dt
      end function fnt4
      complex(C_FLOAT_COMPLEX) function fnt5(dt) bind(c)
         use mxisob27a
         type(dts2), intent(inout) :: dt
      end function fnt5
      complex(C_FLOAT_COMPLEX) function fnt6(dt) bind(c)
         use mxisob27a
         type(dts2), value :: dt
      end function fnt6
      complex(C_FLOAT_COMPLEX) function fnt7(dt) bind(c)
         use mxisob27a
         type(dts0), intent(in) :: dt
      end function fnt7
      complex(C_FLOAT_COMPLEX) function fnt8(dt) bind(c)
         use mxisob27a
         type(dts0), intent(in), value :: dt
      end function fnt8
      complex(C_FLOAT_COMPLEX) function fnt9(dt) bind(c)
         use mxisob27a
         type(dts1), intent(in) :: dt
      end function fnt9
      complex(C_FLOAT_COMPLEX) function fnt10(dt) bind(c)
         use mxisob27a
         type(dts1), intent(in), value :: dt
      end function fnt10
      complex(C_FLOAT_COMPLEX) function fnt11(dt) bind(c)
         use mxisob27a
         type(dts2), intent(in) :: dt
      end function fnt11
      complex(C_FLOAT_COMPLEX) function fnt12(dt) bind(c)
         use mxisob27a
         type(dts2), intent(in), value :: dt
      end function fnt12
      complex(C_FLOAT_COMPLEX) function fnt13(dt) bind(c)
         use mxisob27a
         type(dts0), intent(out) :: dt
      end function fnt13
      complex(C_FLOAT_COMPLEX) function fnt14(dt) bind(c)
         use mxisob27a
         type(dts1), intent(out) :: dt
      end function fnt14
      complex(C_FLOAT_COMPLEX) function fnt15(dt) bind(c)
         use mxisob27a
         type(dts2), intent(out) :: dt
      end function fnt15
   end interface

   type(dts0) :: dta
   type(dts1) :: dtb
   type(dts2) :: dtc
   integer ret

!! Test 1

   call initdts0(dta)

   ret = fnt1(dta)

   do i = 1, 5
      if ( dta%a(i) /= cmplx(i+1,i+1,C_FLOAT_COMPLEX) ) error stop 20
   end do

   do i = 1, 3
      if ( dta%b(i) /= cmplx(i+1,i+1,C_DOUBLE_COMPLEX) ) error stop 22
   end do

!! Test 2

   call initdts0(dta)

   ret = fnt2(dta)

   do i = 1, 5
      if ( dta%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 24
   end do

   do i = 1, 3
      if ( dta%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 26
   end do

!! Test 3

   call initdts1(dtb)

   ret = fnt3(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= cmplx(i*2,i*2,C_FLOAT_COMPLEX) ) error stop 28
      if ( dtb%d0%a(i) /= cmplx(i*2,i*2,C_FLOAT_COMPLEX) ) error stop 30
   end do

   do i = 1, 3
      if ( dtb%b(i) /= cmplx(i*2,i*2,C_DOUBLE_COMPLEX) ) error stop 32
      if ( dtb%d0%b(i) /= cmplx(i*2,i*2,C_DOUBLE_COMPLEX) ) error stop 34
   end do

!! Test 4

   call initdts1(dtb)

   ret = fnt4(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 36
      if ( dtb%d0%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 38
   end do

   do i = 1, 3
      if ( dtb%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 40
      if ( dtb%d0%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 42
   end do

!! Test 5

   call initdts2(dtc)

   ret = fnt5(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= cmplx(i*2,i*2,C_FLOAT_COMPLEX) ) error stop 44
      if ( dtc%d1%a(i) /= cmplx(i*2,i*2,C_FLOAT_COMPLEX) ) error stop 46
      if ( dtc%d1%d0%a(i) /= cmplx(i*2,i*2,C_FLOAT_COMPLEX) ) error stop 48
   end do

   do i = 1, 3
      if ( dtc%b(i) /= cmplx(i*2,i*2,C_DOUBLE_COMPLEX) ) error stop 50
      if ( dtc%d1%b(i) /= cmplx(i*2,i*2,C_DOUBLE_COMPLEX) ) error stop 52
      if ( dtc%d1%d0%b(i) /= cmplx(i*2,i*2,C_DOUBLE_COMPLEX) ) error stop 54
   end do

!! Test 6

   call initdts2(dtc)

   ret = fnt6(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 56
      if ( dtc%d1%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 58
      if ( dtc%d1%d0%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 60
   end do

   do i = 1, 3
      if ( dtc%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 62
      if ( dtc%d1%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 64
      if ( dtc%d1%d0%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 66
   end do

!! Test 7

   call initdts0(dta)

   ret = fnt7(dta)

   do i = 1, 5
      if ( dta%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 68
   end do

   do i = 1, 3
      if ( dta%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 70
   end do

!! Test 8

   call initdts0(dta)

   ret = fnt8(dta)

   do i = 1, 5
      if ( dta%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 72
   end do

   do i = 1, 3
      if ( dta%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 74
   end do

!! Test 9

   call initdts1(dtb)

   ret = fnt9(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 76
      if ( dtb%d0%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 78
   end do

   do i = 1, 3
      if ( dtb%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 80
      if ( dtb%d0%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 82
   end do

!! Test 10

   call initdts1(dtb)

   ret = fnt10(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 84
      if ( dtb%d0%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 86
   end do

   do i = 1, 3
      if ( dtb%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 88
      if ( dtb%d0%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 90
   end do

!! Test 11

   call initdts2(dtc)

   ret = fnt11(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 92
      if ( dtc%d1%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 94
      if ( dtc%d1%d0%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 96
   end do

   do i = 1, 3
      if ( dtc%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 98
      if ( dtc%d1%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 100
      if ( dtc%d1%d0%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 102
   end do

!! Test 12

   call initdts2(dtc)

   ret = fnt12(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 104
      if ( dtc%d1%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 106
      if ( dtc%d1%d0%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 108
   end do

   do i = 1, 3
      if ( dtc%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 110
      if ( dtc%d1%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 112
      if ( dtc%d1%d0%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 114
   end do

!! Test 13

   call initdts0(dta)

   ret = fnt13(dta)

   do i = 1, 5
      if ( dta%a(i) /= cmplx(i*2,i*2,C_FLOAT_COMPLEX) ) error stop 116
   end do

   do i = 1, 3
      if ( dta%b(i) /= cmplx(i*2,i*2,C_DOUBLE_COMPLEX) ) error stop 118
   end do

!! Test 14

   call initdts1(dtb)

   ret = fnt14(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= cmplx(i*2,i*2,C_FLOAT_COMPLEX) ) error stop 120
      if ( dtb%d0%a(i) /= cmplx(i*2,i*2,C_FLOAT_COMPLEX) ) error stop 122
   end do

   do i = 1, 3
      if ( dtb%b(i) /= cmplx(i*2,i*2,C_DOUBLE_COMPLEX) ) error stop 124
      if ( dtb%d0%b(i) /= cmplx(i*2,i*2,C_DOUBLE_COMPLEX) ) error stop 126
   end do

!! Test 15

   call initdts2(dtc)

   ret = fnt15(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= cmplx(i*2,i*2,C_FLOAT_COMPLEX) ) error stop 128
      if ( dtc%d1%a(i) /= cmplx(i*2,i*2,C_FLOAT_COMPLEX) ) error stop 130
      if ( dtc%d1%d0%a(i) /= cmplx(i*2,i*2,C_FLOAT_COMPLEX) ) error stop 132
   end do

   do i = 1, 3
      if ( dtc%b(i) /= cmplx(i*2,i*2,C_DOUBLE_COMPLEX) ) error stop 134
      if ( dtc%d1%b(i) /= cmplx(i*2,i*2,C_DOUBLE_COMPLEX) ) error stop 136
      if ( dtc%d1%d0%b(i) /= cmplx(i*2,i*2,C_DOUBLE_COMPLEX) ) error stop 138
   end do

end program fxison27a

subroutine initdts0(dt)
   use mxisob27a

   type(dts0) :: dt

   do i = 1, 5
      dt%a(i) = cmplx(i,i,C_FLOAT_COMPLEX)
   end do

   do i = 1, 3
      dt%b(i) = cmplx(i,i,C_DOUBLE_COMPLEX)
   end do

end subroutine initdts0

subroutine initdts1(dt)
   use mxisob27a

   type(dts1) :: dt

   do i = 1, 5
      dt%a(i) = cmplx(i,i,C_FLOAT_COMPLEX)
   end do

   do i = 1, 3
      dt%b(i) = cmplx(i,i,C_DOUBLE_COMPLEX)
   end do

   call initdts0(dt%d0)

end subroutine initdts1

subroutine initdts2(dt)
   use mxisob27a

   type(dts2) :: dt

   do i = 1, 5
      dt%a(i) = cmplx(i,i,C_FLOAT_COMPLEX)
   end do

   do i = 1, 3
      dt%b(i) = cmplx(i,i,C_DOUBLE_COMPLEX)
   end do

   call initdts1(dt%d1)

end subroutine initdts2

complex(C_FLOAT_COMPLEX) function fnt1(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts0), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 140
      dt%a(i) = cmplx(i+1,i+1,C_FLOAT_COMPLEX)
   end do

   do i = 1, 3
      if ( dt%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 142
      dt%b(i) = cmplx(i+1,i+1,C_DOUBLE_COMPLEX)
   end do

   fnt1 = 0
end function fnt1

complex(C_FLOAT_COMPLEX) function fnt2(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts0), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 144
      dt%a(i) = cmplx(i+1,i+1,C_FLOAT_COMPLEX)
   end do

   do i = 1, 3
      if ( dt%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 146
      dt%b(i) = cmplx(i+1,i+1,C_DOUBLE_COMPLEX)
   end do

   fnt2 = 0
end function fnt2

complex(C_FLOAT_COMPLEX) function fnt3(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts1), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 148
      dt%a(i) = dt%a(i) + cmplx(i,i,C_FLOAT_COMPLEX)
      if ( dt%d0%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 150
      dt%d0%a(i) = dt%d0%a(i) + cmplx(i,i,C_FLOAT_COMPLEX)
   end do

   do i = 1, 3
      if ( dt%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 152
      dt%b(i) = dt%b(i) + cmplx(i,i,C_DOUBLE_COMPLEX)
      if ( dt%d0%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 154
      dt%d0%b(i) = dt%d0%b(i) + cmplx(i,i,C_DOUBLE_COMPLEX)
   end do

   fnt3 = 0
end function fnt3

complex(C_FLOAT_COMPLEX) function fnt4(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts1), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 156
      dt%a(i) = dt%a(i) + cmplx(i,i,C_FLOAT_COMPLEX)
      if ( dt%d0%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 158
      dt%d0%a(i) = dt%d0%a(i) + cmplx(i,i,C_FLOAT_COMPLEX)
   end do

   do i = 1, 3
      if ( dt%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 160
      dt%b(i) = dt%b(i) + cmplx(i,i,C_DOUBLE_COMPLEX)
      if ( dt%d0%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 162
      dt%d0%b(i) = dt%d0%b(i) + cmplx(i,i,C_DOUBLE_COMPLEX)
   end do

   fnt4 = 0
end function fnt4

complex(C_FLOAT_COMPLEX) function fnt5(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts2), intent(inout) :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 164
      dt%a(i) = dt%a(i) + cmplx(i,i,C_FLOAT_COMPLEX)
      if ( dt%d1%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 166
      dt%d1%a(i) = dt%d1%a(i) + cmplx(i,i,C_FLOAT_COMPLEX)
      if ( dt%d1%d0%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 168
      dt%d1%d0%a(i) = dt%d1%d0%a(i) + cmplx(i,i,C_FLOAT_COMPLEX)
   end do

   do i = 1, 3
      if ( dt%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 170
      dt%b(i) = dt%b(i) + cmplx(i,i,C_DOUBLE_COMPLEX)
      if ( dt%d1%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 172
      dt%d1%b(i) = dt%d1%b(i) + cmplx(i,i,C_DOUBLE_COMPLEX)
      if ( dt%d1%d0%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 174
      dt%d1%d0%b(i) = dt%d1%d0%b(i) + cmplx(i,i,C_DOUBLE_COMPLEX)
   end do

   fnt5 = 0
end function fnt5

complex(C_FLOAT_COMPLEX) function fnt6(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts2), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 176
      dt%a(i) = dt%a(i) + cmplx(i,i,C_FLOAT_COMPLEX)
      if ( dt%d1%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 178
      dt%d1%a(i) = dt%d1%a(i) + cmplx(i,i,C_FLOAT_COMPLEX)
      if ( dt%d1%d0%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 180
      dt%d1%d0%a(i) = dt%d1%d0%a(i) + cmplx(i,i,C_FLOAT_COMPLEX)
   end do

   do i = 1, 3
      if ( dt%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 182
      dt%b(i) = dt%b(i) + cmplx(i,i,C_DOUBLE_COMPLEX)
      if ( dt%d1%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 184
      dt%d1%b(i) = dt%d1%b(i) + cmplx(i,i,C_DOUBLE_COMPLEX)
      if ( dt%d1%d0%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 186
      dt%d1%d0%b(i) = dt%d1%d0%b(i) + cmplx(i,i,C_DOUBLE_COMPLEX)
   end do

   fnt6 = 0
end function fnt6

complex(C_FLOAT_COMPLEX) function fnt7(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts0), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 188
   end do

   do i = 1, 3
      if ( dt%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 190
   end do

   fnt7 = 0
end function fnt7

complex(C_FLOAT_COMPLEX) function fnt8(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts0), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 192
   end do

   do i = 1, 3
      if ( dt%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 194
   end do

   fnt8 = 0
end function fnt8

complex(C_FLOAT_COMPLEX) function fnt9(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts1), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 196
      if ( dt%d0%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 198
   end do

   do i = 1, 3
      if ( dt%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 200
      if ( dt%d0%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 202
   end do

   fnt9 = 0
end function fnt9

complex(C_FLOAT_COMPLEX) function fnt10(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts1), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 204
      if ( dt%d0%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 206
   end do

   do i = 1, 3
      if ( dt%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 208
      if ( dt%d0%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 210
   end do

   fnt10 = 0
end function fnt10

complex(C_FLOAT_COMPLEX) function fnt11(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts2), intent(in) :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 212
      if ( dt%d1%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 214
      if ( dt%d1%d0%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 216
   end do

   do i = 1, 3
      if ( dt%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 218
      if ( dt%d1%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 220
      if ( dt%d1%d0%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 222
   end do

   fnt11 = 0
end function fnt11

complex(C_FLOAT_COMPLEX) function fnt12(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts2), intent(in), value :: dt

   do i = 1, 5
      if ( dt%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 224
      if ( dt%d1%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 226
      if ( dt%d1%d0%a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 228
   end do

   do i = 1, 3
      if ( dt%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 230
      if ( dt%d1%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 232
      if ( dt%d1%d0%b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 234
   end do

   fnt12 = 0
end function fnt12

complex(C_FLOAT_COMPLEX) function fnt13(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts0), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = cmplx(i+i,i+i,C_FLOAT_COMPLEX)
   end do

   do i = 1, 3
      dt%b(i) = cmplx(i+i,i+i,C_DOUBLE_COMPLEX)
   end do

   fnt13 = 0
end function fnt13

complex(C_FLOAT_COMPLEX) function fnt14(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts1), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = dt%a(i) + cmplx(i,i,C_FLOAT_COMPLEX)
      dt%d0%a(i) = dt%d0%a(i) + cmplx(i,i,C_FLOAT_COMPLEX)
   end do

   do i = 1, 3
      dt%b(i) = dt%b(i) + cmplx(i,i,C_DOUBLE_COMPLEX)
      dt%d0%b(i) = dt%d0%b(i) + cmplx(i,i,C_DOUBLE_COMPLEX)
   end do

   fnt14 = 0
end function fnt14

complex(C_FLOAT_COMPLEX) function fnt15(dt) bind(c)
   use ISO_C_BINDING
   use mxisob27a

   type(dts2), intent(out) :: dt

   do i = 1, 5
      dt%a(i) = dt%a(i) + cmplx(i,i,C_FLOAT_COMPLEX)
      dt%d1%a(i) = dt%d1%a(i) + cmplx(i,i,C_FLOAT_COMPLEX)
      dt%d1%d0%a(i) = dt%d1%d0%a(i) + cmplx(i,i,C_FLOAT_COMPLEX)
   end do

   do i = 1, 3
      dt%b(i) = dt%b(i) + cmplx(i,i,C_DOUBLE_COMPLEX)
      dt%d1%b(i) = dt%d1%b(i) + cmplx(i,i,C_DOUBLE_COMPLEX)
      dt%d1%d0%b(i) = dt%d1%d0%b(i) + cmplx(i,i,C_DOUBLE_COMPLEX)
   end do

   fnt15 = 0
end function fnt15
