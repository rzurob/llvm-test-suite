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

module print_number_images

   contains

   subroutine print_num_img()

      print *,"Printing Number of Images",num_images()

   end subroutine print_num_img

end module
