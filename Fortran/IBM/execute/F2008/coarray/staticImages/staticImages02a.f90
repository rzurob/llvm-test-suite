!*  ============================================================================
!*
!*  TEST CASE NAME             : staticImages02a.f
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

module fibonnaci_module

   contains

   subroutine fibonacci()

      integer, parameter :: N = 9
      integer, dimension(N + 1), save :: A[*]
      integer i

      A(1) = 0
      A(2) = 1
      do i = 3, N + 1
         A(i) = A(i - 2) + A(i - 1)
      end do

      print *,"Image",this_image(),":",A(N + 1)

   end subroutine fibonacci

end module

