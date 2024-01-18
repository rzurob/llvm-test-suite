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
!*     Test the use in array coarray
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   implicit none

   integer i
   integer, parameter :: N = 7
   real, save, dimension(N) :: Re[*]
   real, save, dimension(N) :: Im[*]
   complex, save, dimension(N) :: C_A[N/2, *]

   if (this_image() == 1) then
      do i = 1, N
         Re(i) = 1.5 + num_images() + i
         Im(i) = 0.5 + num_images() + i
      end do
   end if
!   SYNC IMAGES(1)                      ! Wait for image 1 to finish
   SYNC ALL

   do i = 1, N
      C_A(i)%RE = Re(i)[1]
      C_A(i)%IM = Im(i)[1]
   end do

   print *,this_image(),":",Re
   print *,this_image(),":",Im
   print *,this_image(),":",C_A

   do i = 1, N
      if (Re(i)[1] .NE. C_A(i)%RE) then
         print *,this_image(),":",Re(i)[1]," .NE. ",C_A(i)%RE
         ERROR STOP 101
      else if (Im(i)[1] .NE. C_A(i)%IM) then
         print *,this_image(),":",Im(i)[1]," .NE. ",C_A(i)%IM
         ERROR STOP 201
      end if
   end do

end program main
