!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa01.presh fxisok25
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
!*      - testing C_CHAR and C_SIGNED_CHAR
!*      - FORTRAN code only
!*      - passing 1-dim and 2-dim array arguments
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisok25
   use ISO_C_BINDING

   interface
      integer(C_SIGNED_CHAR) function fnt1(a,b)
         use ISO_C_BINDING
         character(C_CHAR) :: a(4)
         integer(C_SIGNED_CHAR) :: b(4)
      end function fnt1
      integer(C_SIGNED_CHAR) function fnt2(a,b)
         use ISO_C_BINDING
         character(C_CHAR), intent(in) :: a(4)
         integer(C_SIGNED_CHAR), intent(in) :: b(4)
      end function fnt2
      integer(C_SIGNED_CHAR) function fnt3(a,b)
         use ISO_C_BINDING
         character(C_CHAR), intent(inout) :: a(4)
         integer(C_SIGNED_CHAR), intent(inout) :: b(4)
      end function fnt3
      integer(C_SIGNED_CHAR) function fnt4(a,b)
         use ISO_C_BINDING
         character(C_CHAR), intent(out) :: a(4)
         integer(C_SIGNED_CHAR), intent(out) :: b(4)
      end function fnt4
      integer(C_SIGNED_CHAR) function fnt5(aa,bb)
         use ISO_C_BINDING
         character(C_CHAR) :: aa(6,4)
         integer(C_SIGNED_CHAR) :: bb(6,4)
      end function fnt5
      integer(C_SIGNED_CHAR) function fnt6(aa,bb)
         use ISO_C_BINDING
         character(C_CHAR), intent(in) :: aa(6,4)
         integer(C_SIGNED_CHAR), intent(in) :: bb(6,4)
      end function fnt6
      integer(C_SIGNED_CHAR) function fnt7(aa,bb)
         use ISO_C_BINDING
         character(C_CHAR), intent(inout) :: aa(6,4)
         integer(C_SIGNED_CHAR), intent(inout) :: bb(6,4)
      end function fnt7
      integer(C_SIGNED_CHAR) function fnt8(aa,bb)
         use ISO_C_BINDING
         character(C_CHAR), intent(out) :: aa(6,4)
         integer(C_SIGNED_CHAR), intent(out) :: bb(6,4)
      end function fnt8
   end interface

   character(C_CHAR) :: a(4), aa(6,4)
   integer(C_SIGNED_CHAR) :: b(4), bb(6,4)
   integer :: ret
   integer i, j

!! Test 1

   call initarr1d(a,b)

   ret = fnt1(a,b)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i+3) ) error stop 20
      if ( b(i) /= iachar('A')+i+3 ) error stop 22
   end do

!! Test 2

   call initarr1d(a,b)

   ret = fnt2(a,b)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i-1) ) error stop 24
      if ( b(i) /= iachar('A')+i-1 ) error stop 26
   end do

!! Test 3

   call initarr1d(a,b)

   ret = fnt3(a,b)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i+3) ) error stop 28
      if ( b(i) /= iachar('A')+i+3 ) error stop 30
   end do

!! Test 4

   call initarr1d(a,b)

   ret = fnt4(a,b)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i+3) ) error stop 32
      if ( b(i) /= iachar('A')+i+3 ) error stop 34
   end do

!! Test 5

   call initarr2d(aa,bb)

   ret = fnt5(aa,bb)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+j) ) error stop 36
         if ( bb(j,i) /= iachar('A')+(i-1)*6+j ) error stop 38
      end do
   end do

!! Test 6

   call initarr2d(aa,bb)

   ret = fnt6(aa,bb)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+(j-1)) ) error stop 40
         if ( bb(j,i) /= iachar('A')+(i-1)*6+(j-1) ) error stop 42
      end do
   end do

!! Test 7

   call initarr2d(aa,bb)

   ret = fnt7(aa,bb)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+j) ) error stop 44
         if ( bb(j,i) /= iachar('A')+(i-1)*6+j ) error stop 46
      end do
   end do

!! Test 8

   call initarr2d(aa,bb)

   ret = fnt8(aa,bb)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+j) ) error stop 48
         if ( bb(j,i) /= iachar('A')+(i-1)*6+j ) error stop 50
      end do
   end do

end program fxisok25

subroutine initarr1d(x,y)
   use ISO_C_BINDING

   character(C_CHAR) :: x(4)
   integer(C_SIGNED_CHAR) :: y(4)

   do i = 1, 4
      x(i) = achar(iachar('A')+i-1)
   end do

   do i = 1, 4
      y(i) = iachar('A')+i-1
   end do

end subroutine initarr1d

subroutine initarr2d(xx,yy)
   use ISO_C_BINDING

   character(C_CHAR) :: xx(6,4)
   integer(C_SIGNED_CHAR) :: yy(6,4)

   do i = 1, 4
      do j = 1, 6
         xx(j,i) = achar(iachar('A')+(i-1)*6+(j-1))
      end do
   end do

   do i = 1, 4
      do j = 1, 6
         yy(j,i) = iachar('A')+(i-1)*6+(j-1)
      end do
   end do

end subroutine initarr2d

integer(C_SIGNED_CHAR) function fnt1(a,b)
   use ISO_C_BINDING

   character(C_CHAR) :: a(4)
   integer(C_SIGNED_CHAR) :: b(4)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i-1) ) error stop 52
      a(i) = achar(iachar('A')+i+3)
      if ( b(i) /= iachar('A')+i-1 ) error stop 54
      b(i) = iachar('A')+i+3
   end do

   fnt1 = 0
end function fnt1

integer(C_SIGNED_CHAR) function fnt2(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(in) :: a(4)
   integer(C_SIGNED_CHAR), intent(in) :: b(4)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i-1) ) error stop 56
      if ( b(i) /= iachar('A')+i-1 ) error stop 58
   end do

   fnt2 = 0
end function fnt2

integer(C_SIGNED_CHAR) function fnt3(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(inout) :: a(4)
   integer(C_SIGNED_CHAR), intent(inout) :: b(4)

   do i = 1, 4
      if ( a(i) /= achar(iachar('A')+i-1) ) error stop 60
      a(i) = achar(iachar('A')+i+3)
      if ( b(i) /= iachar('A')+i-1 ) error stop 62
      b(i) = iachar('A')+i+3
   end do

   fnt3 = 0
end function fnt3

integer(C_SIGNED_CHAR) function fnt4(a,b)
   use ISO_C_BINDING

   character(C_CHAR), intent(out) :: a(4)
   integer(C_SIGNED_CHAR), intent(out) :: b(4)

   do i = 1, 4
      a(i) = achar(iachar('A')+i+3)
      b(i) = iachar('A')+i+3
   end do

   fnt4 = 0
end function fnt4

integer(C_SIGNED_CHAR) function fnt5(aa,bb)
   use ISO_C_BINDING

   character(C_CHAR) :: aa(6,4)
   integer(C_SIGNED_CHAR) :: bb(6,4)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+(j-1)) ) error stop 64
         aa(j,i) = achar(iachar('A')+(i-1)*6+j)
         if ( bb(j,i) /= iachar('A')+(i-1)*6+(j-1) ) error stop 66
         bb(j,i) = iachar('A')+(i-1)*6+j
      end do
   end do

   fnt5 = 0
end function fnt5

integer(C_SIGNED_CHAR) function fnt6(aa,bb)
   use ISO_C_BINDING

   character(C_CHAR), intent(in) :: aa(6,4)
   integer(C_SIGNED_CHAR), intent(in) :: bb(6,4)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+(j-1)) ) error stop 68
         if ( bb(j,i) /= iachar('A')+(i-1)*6+(j-1) ) error stop 70
      end do
   end do

   fnt6 = 0
end function fnt6

integer(C_SIGNED_CHAR) function fnt7(aa,bb)
   use ISO_C_BINDING

   character(C_CHAR), intent(inout) :: aa(6,4)
   integer(C_SIGNED_CHAR), intent(inout) :: bb(6,4)

   do i = 1, 4
      do j = 1, 6
         if ( aa(j,i) /= achar(iachar('A')+(i-1)*6+(j-1)) ) error stop 72
         aa(j,i) = achar(iachar('A')+(i-1)*6+j)
         if ( bb(j,i) /= iachar('A')+(i-1)*6+(j-1) ) error stop 74
         bb(j,i) = iachar('A')+(i-1)*6+j
      end do
   end do

   fnt7 = 0
end function fnt7

integer(C_SIGNED_CHAR) function fnt8(aa,bb)
   use ISO_C_BINDING

   character(C_CHAR), intent(out) :: aa(6,4)
   integer(C_SIGNED_CHAR), intent(out) :: bb(6,4)

   do i = 1, 4
      do j = 1, 6
         aa(j,i) = achar(iachar('A')+(i-1)*6+j)
         bb(j,i) = iachar('A')+(i-1)*6+j
      end do
   end do

   fnt8 = 0
end function fnt8
