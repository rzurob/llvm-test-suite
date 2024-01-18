!*  ============================================================================
!*
!*  DATE                       : 2011-01-12
!*
!*  PRIMARY FUNCTIONS TESTED   : Complex Part Designator
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 383634
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  This program tests the complex part designator:
!*     Test if an array section as a complex part designator is contiguous or not
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module m

   contains

   logical function testIfContiguousC(A)

      complex, intent(in), dimension(:) :: A
      testIfContiguousC = is_contiguous(A)
      return

   end function testIfContiguousC

   logical function testIfContiguousR(A)

      real, intent(in), dimension(:) :: A
      testIfContiguousR = is_contiguous(A)
      return

   end function testIfContiguousR

end module m

program main

   use m
   implicit none

   integer i
   integer, parameter :: N = 4*300
   complex, dimension(N)  :: C
   complex, dimension(N/2)  :: C12
   complex, dimension(N/4)  :: C14

   ! Initialize the array
   do i = 1, N
      C(i) = (1.5, 0.5) * i
   end do

   if (.NOT. testIfContiguousC(C)) ERROR STOP 1    ! C is contiguous
   if (testIfContiguousC(C(1:N:2))) ERROR STOP 3   ! Odd elements of C are not contiguous
   if (testIfContiguousR(C%RE)) ERROR STOP 5       ! Real elements of C are not contiguous
   if (testIfContiguousR(C%IM)) ERROR STOP 7       ! Imaginary elements of C are not contiguous

   C12 = C(:N/2)                                   ! Array section of C (first half elements), C12
   if (.NOT. testIfContiguousC(C12)) ERROR STOP 11 ! C12 is contiguous
   if (testIfContiguousR(C12%RE)) ERROR STOP 15    ! Real elements of C12 are not contiguous
   if (testIfContiguousR(C12%IM)) ERROR STOP 17    ! Imaginary elements of C12 are not contiguous

   C12 = C((N/2)+1:)                               ! Array section of C (other half elements), C12
   if (.NOT. testIfContiguousC(C12)) ERROR STOP 21 ! C12 is contiguous
   if (testIfContiguousR(C12%RE)) ERROR STOP 25    ! Real elements of C12 are not contiguous
   if (testIfContiguousR(C12%IM)) ERROR STOP 27    ! Imaginary elements of C12 are not contiguous

   C14 = C((N/2)+1:N/2+N/4)                        ! Array section of C (1/4 elements), C14
   if (.NOT. testIfContiguousC(C14)) ERROR STOP 31 ! C14 is contiguous
   if (testIfContiguousR(C14%RE)) ERROR STOP 35    ! Real elements of C14 are not contiguous
   if (testIfContiguousR(C14%IM)) ERROR STOP 37    ! Imaginary elements of C14 are not contiguous

end program main
