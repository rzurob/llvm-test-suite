!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrison00.presh fxison07 cxison07
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
!*	- testing C_FLOAT_COMPLEX and C_DOUBLE_COMPLEX
!*	- using external FORTRAN subroutines
!*	- passing 1-dim and 2-dim array arguments
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine sub1(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX) :: a(5)
   complex(C_DOUBLE_COMPLEX) :: b(5)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 20
      a(i) = cmplx(i+1,i+1,C_FLOAT_COMPLEX)
      if ( b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 22
      b(i) = cmplx(i+1,i+1,C_DOUBLE_COMPLEX)
   end do

end subroutine sub1

subroutine sub2(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(in) :: a(5)
   complex(C_DOUBLE_COMPLEX), intent(in) :: b(5)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 24
      if ( b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 26
   end do

end subroutine sub2

subroutine sub2a(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(in) :: a(5)
   complex(C_DOUBLE_COMPLEX), intent(in) :: b(5)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 28
      if ( b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 30
   end do

end subroutine sub2a

subroutine sub3(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(inout) :: a(5)
   complex(C_DOUBLE_COMPLEX), intent(inout) :: b(5)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,C_FLOAT_COMPLEX) ) error stop 32
      a(i) = cmplx(i+1,i+1,C_FLOAT_COMPLEX)
      if ( b(i) /= cmplx(i,i,C_DOUBLE_COMPLEX) ) error stop 34
      b(i) = cmplx(i+1,i+1,C_DOUBLE_COMPLEX)
   end do

end subroutine sub3

subroutine sub4(a,b)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(out) :: a(5)
   complex(C_DOUBLE_COMPLEX), intent(out) :: b(5)

   do i = 1, 5
      a(i) = cmplx(i+1,i+1,C_FLOAT_COMPLEX)
      b(i) = cmplx(i+1,i+1,C_DOUBLE_COMPLEX)
   end do

end subroutine sub4

subroutine sub5(aa,bb)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX) :: aa(10,5)
   complex(C_DOUBLE_COMPLEX) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,C_FLOAT_COMPLEX) ) error stop 36
         aa(j,i) = cmplx(i+j,i+j,C_FLOAT_COMPLEX)
         if ( bb(j,i) /= cmplx(i+j-1,i+j-1,C_DOUBLE_COMPLEX) ) error stop 38
         bb(j,i) = cmplx(i+j,i+j,C_DOUBLE_COMPLEX)
      end do
   end do

end subroutine sub5

subroutine sub6(aa,bb)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(in) :: aa(10,5)
   complex(C_DOUBLE_COMPLEX), intent(in) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,C_FLOAT_COMPLEX) ) error stop 40
         if ( bb(j,i) /= cmplx(i+j-1,i+j-1,C_DOUBLE_COMPLEX) ) error stop 42
      end do
   end do

end subroutine sub6

subroutine sub6a(aa,bb)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(in) :: aa(10,5)
   complex(C_DOUBLE_COMPLEX), intent(in) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,C_FLOAT_COMPLEX) ) error stop 44
         if ( bb(j,i) /= cmplx(i+j-1,i+j-1,C_DOUBLE_COMPLEX) ) error stop 46
      end do
   end do

end subroutine sub6a

subroutine sub7(aa,bb)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(inout) :: aa(10,5)
   complex(C_DOUBLE_COMPLEX), intent(inout) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,C_FLOAT_COMPLEX) ) error stop 48
         aa(j,i) = cmplx(i+j,i+j,C_FLOAT_COMPLEX)
         if ( bb(j,i) /= cmplx(i+j-1,i+j-1,C_DOUBLE_COMPLEX) ) error stop 50
         bb(j,i) = cmplx(i+j,i+j,C_DOUBLE_COMPLEX)
      end do
   end do

end subroutine sub7

subroutine sub8(aa,bb)
   use ISO_C_BINDING

   complex(C_FLOAT_COMPLEX), intent(out) :: aa(10,5)
   complex(C_DOUBLE_COMPLEX), intent(out) :: bb(10,5)

   do i = 1, 5
      do j = 1, 10
         aa(j,i) = cmplx(i+j,i+j,C_FLOAT_COMPLEX)
         bb(j,i) = cmplx(i+j,i+j,C_DOUBLE_COMPLEX)
      end do
   end do

end subroutine sub8
