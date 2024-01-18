!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : assumedShapeCoarray16.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2011-02-18
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments assumed shape
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 386330
!*
!*  DRIVER STANZA              : xlf2003
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
!*  Type used integer
!*
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module asc

   implicit none
   integer i

   contains

   subroutine copyData(A, image, next_image)
      real, dimension(:), intent(inout) :: A[*]
      integer, intent(in) :: image, next_image
      A(:)[next_image] = A(:)[image]
      print *,"PRINTING: ",image,":",A(:)[image]
      print *,"PRINTING: ",next_image,":",A(:)[next_image]
   end subroutine copyData

   subroutine computeDataInNext(A, results)
      real, dimension(:), intent(in) :: A
      real, dimension(:), intent(out) :: results
      results = log_gamma(A)
   end subroutine computeDataInNext

   subroutine verifyComputation(A, results, image)
      real, dimension(:), intent(in) :: A
      real, dimension(:), intent(in) :: results
      integer, intent(in) :: image
      do i = 1, SIZE(A)
         if (log_gamma(A(i)) .NE. results(i)) then
            print *,log_gamma(A(i))," .NE. ",results(i)
            ERROR STOP 101
         end if
      end do
      print *,"VERIFIED: ",image,":",log_gamma(A)
      print *,"VERIFIED: ",image,":",results
   end subroutine verifyComputation

end module asc

program main

   use asc

   implicit none
   integer, parameter :: SIZE = 30
   integer x, images, image, next_image

   real, dimension(SIZE), save :: A[*]
   real, dimension(SIZE), save :: results[*]

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
      do x = 1, SIZE
         A(x)[image] = mod(x + image * next_image, SIZE) + 1
      end do

      call copyData(A, image, next_image)
      call computeDataInNext(A(:)[next_image], results(:)[image])

      ! Verify the computation
      call verifyComputation(A(:)[image], results(:)[image], image);
   end if

   SYNC ALL

end program main
