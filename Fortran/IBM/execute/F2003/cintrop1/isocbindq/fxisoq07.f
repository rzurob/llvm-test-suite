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
!*  KEYWORD(S)                 : 16
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*	- testing 16
!*	- using external FORTRAN subroutines
!*	- passing 1-dim and 2-dim array arguments
!*	- main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

subroutine sub1(a)
   use ISO_C_BINDING

   complex(16) :: a(5)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,16) ) error stop 20
      a(i) = cmplx(i+1,i+1,16)
   end do

end subroutine sub1

subroutine sub2(a)
   use ISO_C_BINDING

   complex(16), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,16) ) error stop 22
   end do

end subroutine sub2

subroutine sub2a(a)
   use ISO_C_BINDING

   complex(16), intent(in) :: a(5)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,16) ) error stop 24
   end do

end subroutine sub2a

subroutine sub3(a)
   use ISO_C_BINDING

   complex(16), intent(inout) :: a(5)

   do i = 1, 5
      if ( a(i) /= cmplx(i,i,16) ) error stop 26
      a(i) = cmplx(i+1,i+1,16)
   end do

end subroutine sub3

subroutine sub4(a)
   use ISO_C_BINDING

   complex(16), intent(out) :: a(5)

   do i = 1, 5
      a(i) = cmplx(i+1,i+1,16)
   end do

end subroutine sub4

subroutine sub5(aa)
   use ISO_C_BINDING

   complex(16) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,16) ) error stop 28
         aa(j,i) = cmplx(i+j,i+j,16)
      end do
   end do

end subroutine sub5

subroutine sub6(aa)
   use ISO_C_BINDING

   complex(16), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,16) ) error stop 30
      end do
   end do

end subroutine sub6

subroutine sub6a(aa)
   use ISO_C_BINDING

   complex(16), intent(in) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,16) ) error stop 32
      end do
   end do

end subroutine sub6a

subroutine sub7(aa)
   use ISO_C_BINDING

   complex(16), intent(inout) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         if ( aa(j,i) /= cmplx(i+j-1,i+j-1,16) ) error stop 34
         aa(j,i) = cmplx(i+j,i+j,16)
      end do
   end do

end subroutine sub7

subroutine sub8(aa)
   use ISO_C_BINDING

   complex(16), intent(out) :: aa(10,5)

   do i = 1, 5
      do j = 1, 10
         aa(j,i) = cmplx(i+j,i+j,16)
      end do
   end do

end subroutine sub8