!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisom07 cxisom07
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
!*	- using external FORTRAN subroutines
!*	- passing 1-dim and 2-dim array arguments
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine sub1(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE) :: a(5)

   do i = 1, 5
      if ( a(i) /= real(i,C_LONG_DOUBLE) ) error stop 20
      a(i) = real(i+1,C_LONG_DOUBLE)
   end do

end subroutine sub1

subroutine sub2(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) /= real(i,C_LONG_DOUBLE) ) error stop 22
   end do

end subroutine sub2

subroutine sub2a(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) /= real(i,C_LONG_DOUBLE) ) error stop 24
   end do

end subroutine sub2a

subroutine sub3(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(inout) :: a(5)

   do i = 1, 5
      if ( a(i) /= real(i,C_LONG_DOUBLE) ) error stop 26
      a(i) = real(i+1,C_LONG_DOUBLE)
   end do

end subroutine sub3

subroutine sub4(a)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(out) :: a(5)

   do i = 1, 5
      a(i) = real(i+1,C_LONG_DOUBLE)
   end do

end subroutine sub4

subroutine sub5(aa)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 28
         aa(j,i) = real(i+j,C_LONG_DOUBLE)
      end do
   end do

end subroutine sub5

subroutine sub6(aa)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 30
      end do
   end do

end subroutine sub6

subroutine sub6a(aa)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 32
      end do
   end do

end subroutine sub6a

subroutine sub7(aa)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(inout) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_LONG_DOUBLE) ) error stop 34
         aa(j,i) = real(i+j,C_LONG_DOUBLE)
      end do
   end do

end subroutine sub7

subroutine sub8(aa)
   use ISO_C_BINDING

   real(C_LONG_DOUBLE), intent(out) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         aa(j,i) = real(i+j,C_LONG_DOUBLE)
      end do
   end do

end subroutine sub8
