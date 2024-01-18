!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa01.presh fxisor25
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
!*  KEYWORD(S)                 : C_BOOL
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_BOOL
!*      - FORTRAN code only
!*      - passing 1-dim and 2-dim array arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisor25
   use ISO_C_BINDING

   interface
      logical(C_BOOL) function fnt1(a)
         use ISO_C_BINDING
         logical(C_BOOL) :: a(5)
      end function fnt1
      logical(C_BOOL) function fnt2(a)
         use ISO_C_BINDING
         logical(C_BOOL), intent(in) :: a(5)
      end function fnt2
      logical(C_BOOL) function fnt3(a)
         use ISO_C_BINDING
         logical(C_BOOL), intent(inout) :: a(5)
      end function fnt3
      logical(C_BOOL) function fnt4(a)
         use ISO_C_BINDING
         logical(C_BOOL), intent(out) :: a(5)
      end function fnt4
      logical(C_BOOL) function fnt5(aa)
         use ISO_C_BINDING
         logical(C_BOOL) :: aa(10,5)
      end function fnt5
      logical(C_BOOL) function fnt6(aa)
         use ISO_C_BINDING
         logical(C_BOOL), intent(in) :: aa(10,5)
      end function fnt6
      logical(C_BOOL) function fnt7(aa)
         use ISO_C_BINDING
         logical(C_BOOL), intent(inout) :: aa(10,5)
      end function fnt7
      logical(C_BOOL) function fnt8(aa)
         use ISO_C_BINDING
         logical(C_BOOL), intent(out) :: aa(10,5)
      end function fnt8
   end interface

   logical(C_BOOL) :: a(5), aa(10,5), ret
   integer i, j

!! Test 1

   call initarr1d(a)

   ret = fnt1(a)

   do i = 1, 5
      if ( a(i) .eqv. .true. ) error stop 20
   end do

!! Test 2

   call initarr1d(a)

   ret = fnt2(a)

   do i = 1, 5
      if ( a(i) .neqv. .true. ) error stop 22
   end do

!! Test 3

   call initarr1d(a)

   ret = fnt3(a)

   do i = 1, 5
      if ( a(i) .eqv. .true. ) error stop 24
   end do

!! Test 4

   call initarr1d(a)

   ret = fnt4(a)

   do i = 1, 5
      if ( a(i) .eqv. .true. ) error stop 26
   end do

!! Test 5

   call initarr2d(aa)

   ret = fnt5(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .eqv. .true. ) error stop 28
      end do
   end do

!! Test 6

   call initarr2d(aa)

   ret = fnt6(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .neqv. .true. ) error stop 30
      end do
   end do

!! Test 7

   call initarr2d(aa)

   ret = fnt7(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .eqv. .true. ) error stop 32
      end do
   end do

!! Test 8

   call initarr2d(aa)

   ret = fnt8(aa)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .eqv. .true. ) error stop 34
      end do
   end do

end program fxisor25

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

logical(C_BOOL) function fnt1(a)
   use ISO_C_BINDING

   logical(C_BOOL) :: a(5)

   do i = 1, 5
      if ( a(i) .neqv. .true. ) error stop 36
      a(i) = .not. a(i)
   end do

   fnt1 = .false.
end function fnt1

logical(C_BOOL) function fnt2(a)
   use ISO_C_BINDING

   logical(C_BOOL), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) .neqv. .true. ) error stop 38
   end do

   fnt2 = .false.
end function fnt2

logical(C_BOOL) function fnt3(a)
   use ISO_C_BINDING

   logical(C_BOOL), intent(inout) :: a(5)

   do i = 1, 5
      if ( a(i) .neqv. .true. ) error stop 40
      a(i) = .not. a(i)
   end do

   fnt3 = .false.
end function fnt3

logical(C_BOOL) function fnt4(a)
   use ISO_C_BINDING

   logical(C_BOOL), intent(out) :: a(5)

   do i = 1, 5
      a(i) = .not. a(i)
   end do

   fnt4 = .false.
end function fnt4

logical(C_BOOL) function fnt5(aa)
   use ISO_C_BINDING

   logical(C_BOOL) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .neqv. .true. ) error stop 42
         aa(j,i) = .not. aa(j,i)
      end do
   end do

   fnt5 = .false.
end function fnt5

logical(C_BOOL) function fnt6(aa)
   use ISO_C_BINDING

   logical(C_BOOL), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .neqv. .true. ) error stop 44
      end do
   end do

   fnt6 = .false.
end function fnt6

logical(C_BOOL) function fnt7(aa)
   use ISO_C_BINDING

   logical(C_BOOL), intent(inout) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) .neqv. .true. ) error stop 46
         aa(j,i) = .not. aa(j,i)
      end do
   end do

   fnt7 = .false.
end function fnt7

logical(C_BOOL) function fnt8(aa)
   use ISO_C_BINDING

   logical(C_BOOL), intent(out) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         aa(j,i) = .not. aa(j,i)
      end do
   end do

   fnt8 = .false.
end function fnt8
