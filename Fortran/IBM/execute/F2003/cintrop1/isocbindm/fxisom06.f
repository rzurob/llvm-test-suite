!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisom06 cxisom06
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
!*  KEYWORD(S)                 : C_LONG_DOUBLE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_LONG_DOUBLE
!*	- using external FORTRAN functions
!*	- passing 1-dim and 2-dim array arguments
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

real(C_LONG_DOUBLE) function fnt1(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE) :: a(5)

   do i = 1, 5
      if ( a(i) /= real(i,C_LONG_DOUBLE) ) error stop 20
      a(i) = real(i+1,C_LONG_DOUBLE)
   end do

   fnt1 = 0
end function fnt1

real(C_LONG_DOUBLE) function fnt2(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) /= real(i,C_LONG_DOUBLE) ) error stop 22
   end do

   fnt2 = 0
end function fnt2

real(C_LONG_DOUBLE) function fnt2a(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) /= real(i,C_LONG_DOUBLE) ) error stop 24
   end do

   fnt2a = 0
end function fnt2a

real(C_LONG_DOUBLE) function fnt3(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(inout) :: a(5)

   do i = 1, 5
      if ( a(i) /= real(i,C_LONG_DOUBLE) ) error stop 26
      a(i) = real(i+1,C_LONG_DOUBLE)
   end do

   fnt3 = 0
end function fnt3

real(C_LONG_DOUBLE) function fnt4(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(out) :: a(5)

   do i = 1, 5
      a(i) = real(i+1,C_LONG_DOUBLE)
   end do

   fnt4 = 0
end function fnt4

real(C_LONG_DOUBLE) function fnt5(aa)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 28
         aa(j,i) = real(i+j,C_LONG_DOUBLE)
      end do
   end do

   fnt5 = 0
end function fnt5

real(C_LONG_DOUBLE) function fnt6(aa)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 30
      end do
   end do

   fnt6 = 0
end function fnt6

real(C_LONG_DOUBLE) function fnt6a(aa)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 32
      end do
   end do

   fnt6a = 0
end function fnt6a

real(C_LONG_DOUBLE) function fnt7(aa)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(inout) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 34
         aa(j,i) = real(i+j,C_LONG_DOUBLE)
      end do
   end do

   fnt7 = 0
end function fnt7

real(C_LONG_DOUBLE) function fnt8(aa)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(out) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         aa(j,i) = real(i+j,C_LONG_DOUBLE)
      end do
   end do

   fnt8 = 0
end function fnt8
