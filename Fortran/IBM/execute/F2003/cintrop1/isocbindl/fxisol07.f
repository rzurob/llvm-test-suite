!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisol07 cxisol07
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
!*  KEYWORD(S)                 : C_FLOAT, C_DOUBLE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing C_FLOAT and C_DOUBLE
!*	- using external FORTRAN subroutines
!*	- passing 1-dim and 2-dim array arguments
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine sub1(a,b)
   use ISO_C_BINDING

   real(C_FLOAT) :: a(5)
   real(C_DOUBLE) :: b(5)

   do i = 1, 5
      if ( a(i) /= real(i,C_FLOAT) ) error stop 20
      a(i) = real(i+1,C_FLOAT)
      if ( b(i) /= real(i,C_DOUBLE) ) error stop 22
      b(i) = real(i+1,C_DOUBLE)
   end do

end subroutine sub1

subroutine sub2(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), intent(in) :: a(5)
   real(C_DOUBLE), intent(in) :: b(5)

   do i = 1, 5
      if ( a(i) /= real(i,C_FLOAT) ) error stop 24
      if ( b(i) /= real(i,C_DOUBLE) ) error stop 26
   end do

end subroutine sub2

subroutine sub2a(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), intent(in) :: a(5)
   real(C_DOUBLE), intent(in) :: b(5)

   do i = 1, 5
      if ( a(i) /= real(i,C_FLOAT) ) error stop 28
      if ( b(i) /= real(i,C_DOUBLE) ) error stop 30
   end do

end subroutine sub2a

subroutine sub3(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), intent(inout) :: a(5)
   real(C_DOUBLE), intent(inout) :: b(5)

   do i = 1, 5
      if ( a(i) /= real(i,C_FLOAT) ) error stop 32
      a(i) = real(i+1,C_FLOAT)
      if ( b(i) /= real(i,C_DOUBLE) ) error stop 34
      b(i) = real(i+1,C_DOUBLE)
   end do

end subroutine sub3

subroutine sub4(a,b)
   use ISO_C_BINDING

   real(C_FLOAT), intent(out) :: a(5)
   real(C_DOUBLE), intent(out) :: b(5)

   do i = 1, 5
      a(i) = real(i+1,C_FLOAT)
      b(i) = real(i+1,C_DOUBLE)
   end do

end subroutine sub4

subroutine sub5(aa,bb)
   use ISO_C_BINDING

   real(C_FLOAT) :: aa(10,5)
   real(C_DOUBLE) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_FLOAT) ) error stop 36
         aa(j,i) = real(i+j,C_FLOAT)
         if ( bb(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 38
         bb(j,i) = real(i+j,C_DOUBLE)
      end do
   end do

end subroutine sub5

subroutine sub6(aa,bb)
   use ISO_C_BINDING

   real(C_FLOAT), intent(in) :: aa(10,5)
   real(C_DOUBLE), intent(in) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_FLOAT) ) error stop 40
         if ( bb(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 42
      end do
   end do

end subroutine sub6

subroutine sub6a(aa,bb)
   use ISO_C_BINDING

   real(C_FLOAT), intent(in) :: aa(10,5)
   real(C_DOUBLE), intent(in) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_FLOAT) ) error stop 44
         if ( bb(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 46
      end do
   end do

end subroutine sub6a

subroutine sub7(aa,bb)
   use ISO_C_BINDING

   real(C_FLOAT), intent(inout) :: aa(10,5)
   real(C_DOUBLE), intent(inout) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= real(i+j-1,C_FLOAT) ) error stop 48
         aa(j,i) = real(i+j,C_FLOAT)
         if ( bb(j,i) /= real(i+j-1,C_DOUBLE) ) error stop 50
         bb(j,i) = real(i+j,C_DOUBLE)
      end do
   end do

end subroutine sub7

subroutine sub8(aa,bb)
   use ISO_C_BINDING

   real(C_FLOAT), intent(out) :: aa(10,5)
   real(C_DOUBLE), intent(out) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         aa(j,i) = real(i+j,C_FLOAT)
         bb(j,i) = real(i+j,C_DOUBLE)
      end do
   end do

end subroutine sub8
