!*  ============================================================================
!*
!*  DATE                       : 2011-01-19
!*
!*  PRIMARY FUNCTIONS TESTED   : Complex Part Designator
!*
!*  SECONDARY FUNCTIONS TESTED : In CAF Environment
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
!*  This program tests the complex part designator in CAF environment:
!*     Test the use in array coindexed objects
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   implicit none

   integer i
   integer j
   integer, parameter :: N = 8
   complex, save, dimension(N) :: C[*]
   complex, save, dimension(N) :: C_A[*]

   if (this_image() == 1) then
      do i = 1, num_images()
         do j = 1, N
            C(j)[i] = (1.5 + num_images() + j, 0.5 + num_images() + j)
            C_A(j)[i]%RE = 1.5 + num_images() + j
            C_A(j)[i]%IM = 0.5 + num_images() + j
         end do
      end do
   end if
!   SYNC IMAGES(1)                      ! Wait for the image 1 to finish
   SYNC ALL

   print *,this_image(),":",C
   print *,this_image(),":",C_A%RE
   print *,this_image(),":",C_A%IM

   do j = 1, N
      if (C_A(j)%RE .NE. real(C(j))) then
         print *,this_image(),":",C_A(j)%RE," .NE. ",real(C(j))
         ERROR STOP 101
      else if (C_A(j)%IM .NE. imag(C(j))) then
         print *,this_image(),":",C_A(j)%IM," .NE. ",imag(C(j))
         ERROR STOP 201
      end if
   end do

end program main
