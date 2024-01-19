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

module print_image

   contains

   subroutine print_img()

      print *,"Printing Image",this_image()

   end subroutine print_img

end module

