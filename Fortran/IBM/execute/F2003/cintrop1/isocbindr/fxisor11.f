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
!*  KEYWORD(S)                 : C_BOOL
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_BOOL
!*      - using C functions with interface to FORTRAN subroutines
!*      - subroutine interfaces defined in a module
!*      - passing 1-dim and 2-dim array arguments
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob11
   interface
      subroutine sub1(a)
         use ISO_C_BINDING
         logical(C_BOOL) :: a(5)
      end subroutine sub1
      subroutine sub2(a)
         use ISO_C_BINDING
         logical(C_BOOL), intent(in) :: a(5)
      end subroutine sub2
      subroutine sub2a(a)
         use ISO_C_BINDING
         logical(C_BOOL), intent(in) :: a(5)
      end subroutine sub2a
      subroutine sub3(a)
         use ISO_C_BINDING
         logical(C_BOOL), intent(inout) :: a(5)
      end subroutine sub3
      subroutine sub4(a)
         use ISO_C_BINDING
         logical(C_BOOL), intent(out) :: a(5)
      end subroutine sub4
      subroutine sub5(aa)
         use ISO_C_BINDING
         logical(C_BOOL) :: aa(10,5)
      end subroutine sub5
      subroutine sub6(aa)
         use ISO_C_BINDING
         logical(C_BOOL), intent(in) :: aa(10,5)
      end subroutine sub6
      subroutine sub6a(aa)
         use ISO_C_BINDING
         logical(C_BOOL), intent(in) :: aa(10,5)
      end subroutine sub6a
      subroutine sub7(aa)
         use ISO_C_BINDING
         logical(C_BOOL), intent(inout) :: aa(10,5)
      end subroutine sub7
      subroutine sub8(aa)
         use ISO_C_BINDING
         logical(C_BOOL), intent(out) :: aa(10,5)
      end subroutine sub8
   end interface
end module mxisob11

program fxisor11
   use ISO_C_BINDING
   use mxisob11

   logical(C_BOOL) :: a(5), aa(10,5), ret
   integer i, j

!! Test 1

   call initarr1d(a)

   call sub1(a)

   do i = 1, 5
      if ( a(i) .eqv. .true. ) error stop 20
   end do

!! Test 2

   call initarr1d(a)

   call sub2(a)

   do i = 1, 5
      if ( a(i) .neqv. .true. ) error stop 22
   end do

!! Test 2a

   call initarr1d(a)

   call sub2a(a)

   do i = 1, 5
      if ( a(i) .neqv. .true. ) error stop 24
   end do

!! Test 3

   call initarr1d(a)

   call sub3(a)

   do i = 1, 5
      if ( a(i) .eqv. .true. ) error stop 26
   end do

!! Test 4

   call initarr1d(a)

   call sub4(a)

   do i = 1, 5
      if ( a(i) .eqv. .true. ) error stop 28
   end do

!! Test 5

   call initarr2d(aa)

   call sub5(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .eqv. .true. ) error stop 30
      end do
   end do

!! Test 6

   call initarr2d(aa)

   call sub6(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .neqv. .true. ) error stop 32
      end do
   end do

!! Test 6a

   call initarr2d(aa)

   call sub6a(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .neqv. .true. ) error stop 34
      end do
   end do

!! Test 7

   call initarr2d(aa)

   call sub7(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .eqv. .true. ) error stop 36
      end do
   end do

!! Test 8

   call initarr2d(aa)

   call sub8(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .eqv. .true. ) error stop 38
      end do
   end do

end program fxisor11

subroutine initarr1d(x)
   use ISO_C_BINDING

   logical(C_BOOL) :: x(5)

   do i = 1, 5
      x(i) = .true.
   end do


end subroutine initarr1d

subroutine initarr2d(xx)
   use ISO_C_BINDING

   logical(C_BOOL) :: xx(10,5)

   do i = 1, 5
      do j = 1, 10
         xx(j,i) = .true.
      end do
   end do


end subroutine initarr2d
