!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 4/23/2002
!*
!*  PRIMARY FUNCTIONS TESTED   : ISO_C_BINDING module
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  TARGET(S)                  :
!*  KEYWORD(S)                 : 16
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing 16
!*      - using C functions with interfaces to FORTRAN subroutines
!*      - subroutine interfaces defined in module
!*      - passing derived types with 1-dim array fields as arguments
!*      - testing INTENT and VALUE attributes
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob23a
   use ISO_C_BINDING

   type, bind(c) :: dts0
      real(16) :: a(5)
   end type

   type, bind(c) :: dts1
      real(16) :: a(5)
      type(dts0) :: d0
   end type

   type, bind(c) :: dts2
      real(16) :: a(5)
      type(dts1) :: d1
   end type

end module mxisob23a

module mxisob23b
   interface
      subroutine sub1(dt) bind(c)
         use mxisob23a
         type(dts0), intent(inout) :: dt
      end subroutine sub1
      subroutine sub2(dt) bind(c)
         use mxisob23a
         type(dts0), value :: dt
      end subroutine sub2
      subroutine sub3(dt) bind(c)
         use mxisob23a
         type(dts1), intent(inout) :: dt
      end subroutine sub3
      subroutine sub4(dt) bind(c)
         use mxisob23a
         type(dts1), value :: dt
      end subroutine sub4
      subroutine sub5(dt) bind(c)
         use mxisob23a
         type(dts2), intent(inout) :: dt
      end subroutine sub5
      subroutine sub6(dt) bind(c)
         use mxisob23a
         type(dts2), value :: dt
      end subroutine sub6
      subroutine sub7(dt) bind(c)
         use mxisob23a
         type(dts0), intent(in) :: dt
      end subroutine sub7
      subroutine sub7a(dt) bind(c)
         use mxisob23a
         type(dts0), intent(in) :: dt
      end subroutine sub7a
      subroutine sub8(dt) bind(c)
         use mxisob23a
         type(dts0), intent(in), value :: dt
      end subroutine sub8
      subroutine sub8a(dt) bind(c)
         use mxisob23a
         type(dts0), intent(in), value :: dt
      end subroutine sub8a
      subroutine sub9(dt) bind(c)
         use mxisob23a
         type(dts1), intent(in) :: dt
      end subroutine sub9
      subroutine sub9a(dt) bind(c)
         use mxisob23a
         type(dts1), intent(in) :: dt
      end subroutine sub9a
      subroutine sub10(dt) bind(c)
         use mxisob23a
         type(dts1), intent(in), value :: dt
      end subroutine sub10
      subroutine sub10a(dt) bind(c)
         use mxisob23a
         type(dts1), intent(in), value :: dt
      end subroutine sub10a
      subroutine sub11(dt) bind(c)
         use mxisob23a
         type(dts2), intent(in) :: dt
      end subroutine sub11
      subroutine sub11a(dt) bind(c)
         use mxisob23a
         type(dts2), intent(in) :: dt
      end subroutine sub11a
      subroutine sub12(dt) bind(c)
         use mxisob23a
         type(dts2), intent(in), value :: dt
      end subroutine sub12
      subroutine sub12a(dt) bind(c)
         use mxisob23a
         type(dts2), intent(in), value :: dt
      end subroutine sub12a
      subroutine sub13(dt) bind(c)
         use mxisob23a
         type(dts0), intent(out) :: dt
      end subroutine sub13
      subroutine sub14(dt) bind(c)
         use mxisob23a
         type(dts1), intent(out) :: dt
      end subroutine sub14
      subroutine sub15(dt) bind(c)
         use mxisob23a
         type(dts2), intent(out) :: dt
      end subroutine sub15
   end interface
end module mxisob23b

program fxisop23a
   use ISO_C_BINDING
   use mxisob23a
   use mxisob23b

   type(dts0) :: dta
   type(dts1) :: dtb
   type(dts2) :: dtc
   integer ret

!! Test 1

   call initdts0(dta)

   call sub1(dta)

   do i = 1, 5
      if ( dta%a(i) /= real(i+1,16) ) error stop 20
   end do


!! Test 2

   call initdts0(dta)

   call sub2(dta)

   do i = 1, 5
      if ( dta%a(i) /= real(i,16) ) error stop 22
   end do


!! Test 3

   call initdts1(dtb)

   call sub3(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= real(i*2,16) ) error stop 24
      if ( dtb%d0%a(i) /= real(i*2,16) ) error stop 26
   end do


!! Test 4

   call initdts1(dtb)

   call sub4(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= real(i,16) ) error stop 28
      if ( dtb%d0%a(i) /= real(i,16) ) error stop 30
   end do


!! Test 5

   call initdts2(dtc)

   call sub5(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= real(i*2,16) ) error stop 32
      if ( dtc%d1%a(i) /= real(i*2,16) ) error stop 34
      if ( dtc%d1%d0%a(i) /= real(i*2,16) ) error stop 36
   end do


!! Test 6

   call initdts2(dtc)

   call sub6(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= real(i,16) ) error stop 38
      if ( dtc%d1%a(i) /= real(i,16) ) error stop 40
      if ( dtc%d1%d0%a(i) /= real(i,16) ) error stop 42
   end do


!! Test 7

   call initdts0(dta)

   call sub7(dta)

   do i = 1, 5
      if ( dta%a(i) /= real(i,16) ) error stop 44
   end do


!! Test 7a

   call initdts0(dta)

   call sub7a(dta)

   do i = 1, 5
      if ( dta%a(i) /= real(i,16) ) error stop 46
   end do


!! Test 8

   call initdts0(dta)

   call sub8(dta)

   do i = 1, 5
      if ( dta%a(i) /= real(i,16) ) error stop 48
   end do


!! Test 8a

   call initdts0(dta)

   call sub8a(dta)

   do i = 1, 5
      if ( dta%a(i) /= real(i,16) ) error stop 50
   end do


!! Test 9

   call initdts1(dtb)

   call sub9(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= real(i,16) ) error stop 52
      if ( dtb%d0%a(i) /= real(i,16) ) error stop 54
   end do


!! Test 9a

   call initdts1(dtb)

   call sub9a(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= real(i,16) ) error stop 56
      if ( dtb%d0%a(i) /= real(i,16) ) error stop 58
   end do


!! Test 10

   call initdts1(dtb)

   call sub10(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= real(i,16) ) error stop 60
      if ( dtb%d0%a(i) /= real(i,16) ) error stop 62
   end do


!! Test 10a

   call initdts1(dtb)

   call sub10a(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= real(i,16) ) error stop 64
      if ( dtb%d0%a(i) /= real(i,16) ) error stop 66
   end do


!! Test 11

   call initdts2(dtc)

   call sub11(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= real(i,16) ) error stop 68
      if ( dtc%d1%a(i) /= real(i,16) ) error stop 70
      if ( dtc%d1%d0%a(i) /= real(i,16) ) error stop 72
   end do


!! Test 11a

   call initdts2(dtc)

   call sub11a(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= real(i,16) ) error stop 74
      if ( dtc%d1%a(i) /= real(i,16) ) error stop 76
      if ( dtc%d1%d0%a(i) /= real(i,16) ) error stop 78
   end do


!! Test 12

   call initdts2(dtc)

   call sub12(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= real(i,16) ) error stop 80
      if ( dtc%d1%a(i) /= real(i,16) ) error stop 82
      if ( dtc%d1%d0%a(i) /= real(i,16) ) error stop 84
   end do


!! Test 12a

   call initdts2(dtc)

   call sub12a(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= real(i,16) ) error stop 86
      if ( dtc%d1%a(i) /= real(i,16) ) error stop 88
      if ( dtc%d1%d0%a(i) /= real(i,16) ) error stop 90
   end do


!! Test 13

   call initdts0(dta)

   call sub13(dta)

   do i = 1, 5
      if ( dta%a(i) /= real(i*2,16) ) error stop 92
   end do


!! Test 14

   call initdts1(dtb)

   call sub14(dtb)

   do i = 1, 5
      if ( dtb%a(i) /= real(i*2,16) ) error stop 94
      if ( dtb%d0%a(i) /= real(i*2,16) ) error stop 96
   end do


!! Test 15

   call initdts2(dtc)

   call sub15(dtc)

   do i = 1, 5
      if ( dtc%a(i) /= real(i*2,16) ) error stop 98
      if ( dtc%d1%a(i) /= real(i*2,16) ) error stop 100
      if ( dtc%d1%d0%a(i) /= real(i*2,16) ) error stop 102
   end do


end program fxisop23a

subroutine initdts0(dt)
   use mxisob23a

   type(dts0) :: dt

   do i = 1, 5
      dt%a(i) = real(i,16)
   end do


end subroutine initdts0

subroutine initdts1(dt)
   use mxisob23a

   type(dts1) :: dt

   do i = 1, 5
      dt%a(i) = real(i,16)
   end do


   call initdts0(dt%d0)

end subroutine initdts1

subroutine initdts2(dt)
   use mxisob23a

   type(dts2) :: dt

   do i = 1, 5
      dt%a(i) = real(i,16)
   end do


   call initdts1(dt%d1)

end subroutine initdts2
