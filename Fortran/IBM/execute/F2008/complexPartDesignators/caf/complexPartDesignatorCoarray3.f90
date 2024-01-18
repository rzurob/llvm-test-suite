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
!*     Test the use in scalar coarray and coindexed objects
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   implicit none

   integer i
   complex, save :: C1[*], C2[*]

   if (this_image() == 1) then
      do i = 1, num_images()
         C1[i]%RE = 1.5 + num_images()
         C1[i]%IM = 0.5 + num_images()
      end do
      C2%RE = 1.5 + num_images() + 2
      C2%IM = 0.5 + num_images() + 2
   end if
!   SYNC IMAGES(1)                      ! Wait for the image 1 to finish
   SYNC ALL
   C2 = C2[1]

   print *,this_image(),":",C1%RE
   print *,this_image(),":",C1%IM
   print *,this_image(),":",C2

   if (C1%RE .NE. real(C1)) then
      print *,this_image(),":",C1%RE," .NE. ",real(C1)
      ERROR STOP 101
   else if (C1%IM .NE. imag(C1)) then
      print *,this_image(),":",C1%IM," .NE. ",imag(C1)
      ERROR STOP 201
   end if

   if (C2%RE .NE. real(C2[1])) then
      print *,this_image(),":",C2%RE," .NE. ",real(C2[1])
      ERROR STOP 301
   else if (C2%IM .NE. imag(C2[1])) then
      print *,this_image(),":",C2%IM," .NE. ",imag(C2[1])
      ERROR STOP 401
   end if

end program main
