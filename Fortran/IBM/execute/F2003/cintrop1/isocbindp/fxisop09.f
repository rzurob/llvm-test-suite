!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisop00.presh fxisop09 cxisop08
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
!*      - using C functions with interface to FORTRAN functions
!*      - function interfaces defined in a module
!*      - passing 1-dim and 2-dim array arguments
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob09
   interface
      real(16) function fnt1(a)
         use ISO_C_BINDING
         real(16) :: a(5)
      end function fnt1
      real(16) function fnt2(a)
         use ISO_C_BINDING
         real(16), intent(in) :: a(5)
      end function fnt2
      real(16) function fnt2a(a)
         use ISO_C_BINDING
         real(16), intent(in) :: a(5)
      end function fnt2a
      real(16) function fnt3(a)
         use ISO_C_BINDING
         real(16), intent(inout) :: a(5)
      end function fnt3
      real(16) function fnt4(a)
         use ISO_C_BINDING
         real(16), intent(out) :: a(5)
      end function fnt4
      real(16) function fnt5(aa)
         use ISO_C_BINDING
         real(16) :: aa(10,5)
      end function fnt5
      real(16) function fnt6(aa)
         use ISO_C_BINDING
         real(16), intent(in) :: aa(10,5)
      end function fnt6
      real(16) function fnt6a(aa)
         use ISO_C_BINDING
         real(16), intent(in) :: aa(10,5)
      end function fnt6a
      real(16) function fnt7(aa)
         use ISO_C_BINDING
         real(16), intent(inout) :: aa(10,5)
      end function fnt7
      real(16) function fnt8(aa)
         use ISO_C_BINDING
         real(16), intent(out) :: aa(10,5)
      end function fnt8
   end interface
end module mxisob09

program fxisop09
   use ISO_C_BINDING
   use mxisob09

   real(16) :: a(5), aa(10,5), ret
   integer i, j

!! Test 1

   call initarr1d(a)

   ret = fnt1(a)

   do i = 1, 5
      if ( a(i) /= real(i+1,16) ) error stop 20
   end do

!! Test 2

   call initarr1d(a)

   ret = fnt2(a)

   do i = 1, 5
      if ( a(i) /= real(i,16) ) error stop 22
   end do

!! Test 2a

   call initarr1d(a)

   ret = fnt2a(a)

   do i = 1, 5
      if ( a(i) /= real(i,16) ) error stop 24
   end do

!! Test 3

   call initarr1d(a)

   ret = fnt3(a)

   do i = 1, 5
      if ( a(i) /= real(i+1,16) ) error stop 26
   end do

!! Test 4

   call initarr1d(a)

   ret = fnt4(a)

   do i = 1, 5
      if ( a(i) /= real(i+1,16) ) error stop 28
   end do

!! Test 5

   call initarr2d(aa)

   ret = fnt5(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j,16) ) error stop 30
      end do
   end do

!! Test 6

   call initarr2d(aa)

   ret = fnt6(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,16) ) error stop 32
      end do
   end do

!! Test 6a

   call initarr2d(aa)

   ret = fnt6a(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,16) ) error stop 34
      end do
   end do

!! Test 7

   call initarr2d(aa)

   ret = fnt7(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j,16) ) error stop 36
      end do
   end do

!! Test 8

   call initarr2d(aa)

   ret = fnt8(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j,16) ) error stop 38
      end do
   end do

end program fxisop09

subroutine initarr1d(x)
   use ISO_C_BINDING

   real(16) :: x(5)

   do i = 1, 5
      x(i) = real(i,16)
   end do


end subroutine initarr1d

subroutine initarr2d(xx)
   use ISO_C_BINDING

   real(16) :: xx(10,5)

   do i = 1, 5
      do j = 1, 10
         xx(j,i) = real(i+j-1,16)
      end do
   end do


end subroutine initarr2d

