!*  ============================================================================
!*
!*  TEST CASE NAME             : assumedShapeCoarray11.f
!*
!*  DATE                       : 2011-02-18
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments assumed shape
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 386330
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  This program tests the assumed shape dummy arguments coarrays
!*
!*  Copying/moving data among images. Works with only multiple of 3 images.
!*  The extra or less number of images will be ignored.
!*  Image transfers 1/3 of it's array to it's left and right neighbors.
!*  Type used integer
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module asc

   implicit none
   integer i

   contains

   subroutine copyDataToNeighbors(A, image, left_image, right_image)
      integer, dimension(:), intent(inout) :: A[*]
      integer, intent(in) :: image, left_image, right_image
      integer :: S
      S = SIZE(A)
      A(1:S/3)[left_image] = A(1:S/3)[image]
      A(S-S/3+1:S)[right_image] = A(S-S/3+1:S)[image]
      print *,"PRINTING: ",left_image,":",A(:)[left_image]
      print *,"PRINTING: ",image,":",A(:)[image]
      print *,"PRINTING: ",right_image,":",A(:)[right_image]
   end subroutine copyDataToNeighbors

   subroutine verifyCopy(A, image, left_image, right_image)
      integer, dimension(:), intent(in) :: A[*]
      integer, intent(in) :: image, left_image, right_image
      integer :: S
      S = SIZE(A)
      do i = 1, S/3
         if (A(i)[left_image] .NE. A(i)[image]) then
            print *,A(i)[left_image]," .NE. ",A(i)[image]
            ERROR STOP 101
         else if (A(i+S-S/3)[right_image] .NE. A(i+S-S/3)[image]) then
            print *,A(i+S-S/3)[right_image]," NE ",A(i+S-S/3)[image]
            ERROR STOP 102
         end if
      end do
      print *,"VERIFIED: ",left_image,":",A(:)[left_image]
      print *,"VERIFIED: ",image,":",A(:)[image]
      print *,"VERIFIED: ",right_image,":",A(:)[right_image]
   end subroutine verifyCopy

end module asc

program main

   use asc

   implicit none
   integer, parameter :: SIZE = 21
   integer x, images, image, left_image, right_image

   integer, dimension(SIZE), save :: A[*]

   images = num_images()
   image = this_image()
   if (mod(image, 3) .EQ. 0) then
      image = image - 1
      right_image = image + 1
      left_image = image - 1
      !
      ! Initialization of the data
      !
      do x = 1, SIZE
         A(x)[image] = x + image - 1
      end do

      call copyDataToNeighbors(A, image, left_image, right_image)

      ! Verify the copy
      call verifyCopy(A, image, left_image, right_image);
   end if

   SYNC ALL

end program main
