!*  ============================================================================
!*
!*  TEST CASE NAME             : assumedShapeCoarray19.f
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
!*  Passing of arrays to other intrinsic functions. Uses the same corank
!*  but different shapes
!*  Copying/moving data among images. Works with only even number of images.
!*  The extra or less number of images will be ignored.
!*  Images 1,3,... uses image 2,4,... to compute the values.
!*  After the computation is complete the data is passed back to be verified.
!*  We are using different trignometric functions to compute the values.
!*  Type used real
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module asc

   implicit none

   contains

   subroutine copyData(A, image, next_image)
      real, dimension(:,:,:), intent(inout) :: A[*]
      integer, intent(in) :: image, next_image
      A(:,:,:)[next_image] = A(:,:,:)[image]
      print *,"PRINTING: ",image,":",A(:,:,:)[image]
      print *,"PRINTING: ",next_image,":",A(:,:,:)[next_image]
   end subroutine copyData

   subroutine computeDataInNext(A, results, s1, s2, s3)
      real, dimension(:,:,:), intent(in) :: A
      real, dimension(:,:,:), intent(out) :: results
      integer, intent(in) :: s1, s2, s3
      results(:s1,:s2,:s3) = log_gamma(A(:s1,:s2,:s3))
   end subroutine computeDataInNext

   subroutine verifyComputation(A, results, image, s1, s2, s3)
      real, dimension(:,:,:), intent(in) :: A
      real, dimension(:,:,:), intent(in) :: results
      integer, intent(in) :: image, SIZE, s1, s2, s3
      integer x, y, z
      do x = 1, s1
         do y = 1, s2
            do z = 1, s3
               if (log_gamma(A(x,y,z)) .NE. results(x,y,z)) then
                  print *,log_gamma(A(x,y,z))," .NE. ",results(x,y,z)
                  ERROR STOP 101
               end if
            end do
         end do
      end do
      print *,"VERIFIED: ",image,":",log_gamma(A)
      print *,"VERIFIED: ",image,":",results
   end subroutine verifyComputation

end module asc

program main

   use asc

   implicit none
   integer, parameter :: SIZE = 10
   integer x, y, z, images, image, next_image, s1, s2, s3

   real, dimension(SIZE, SIZE, SIZE), save :: A[*]
   real, dimension(SIZE, SIZE, SIZE), save :: results[*]

   s1 = SIZE/2
   s2 = SIZE/5
   s3 = SIZE - SIZE/5
   images = num_images()
   image = this_image()
   ! image is odd (1,3,5,7,......)
   ! next_image is even (2,4,6,8,......)
   if (mod(image, 2) .EQ. 0) then
      next_image = image
      image = image - 1
      !
      ! Initialization of the data
      !
      do x = 1, s1
         do y = 1, s2
            do z = 1, s3
               A(x,y,z)[image] = mod(x + image * next_image, SIZE) + 1
            end do
         end do
      end do

      call copyData(A, image, next_image)
      call computeDataInNext(A(:s1,:s2,:s3)[next_image], results(:,:,:)[image], s1, s2, s3)

      ! Verify the computation
      call verifyComputation(A(:s1,:s2,:s3)[image], results(:s1,:s2,:s3)[image], image, s1, s2, s3);
   end if

   SYNC ALL

end program main
