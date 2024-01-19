!*  ============================================================================
!*
!*  DATE                       : 2011-01-20
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF Static number of Images
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 385616
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  This program tests the static number of images in the CAF environment
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   implicit none

   integer, save :: c_i[*]              ! coarray

   if (this_image() .EQ. 1) then
      c_i = 3001
      SYNC IMAGES(*)                    ! Wait for other images to finish
   else
      SYNC IMAGES(1)                    ! Wait for image 1 to finish
      c_i = c_i[1] + this_image() - 1
   end if
   print *,this_image(),":",c_i

end program main
