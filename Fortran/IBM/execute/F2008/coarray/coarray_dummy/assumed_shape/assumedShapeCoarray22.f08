!*  ============================================================================
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
!*  Passing of arrays to other intrinsic functions. Uses different coranks and
!*  different shapes
!*  Copying/moving data among images. Works with only even number of images.
!*  The extra or less number of images will be ignored.
!*  Images 1,3,... uses image 2,4,... to compute the values.
!*  After the computation is complete the data is passed back to be verified.
!*  We are using different trignometric functions to compute the values.
!*  Type used complex
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module asc

   implicit none
   integer i

   contains

   subroutine copyData(A, image, next_image)
      complex, dimension(:), intent(inout) :: A[1, 1, *]
      integer, intent(in) :: image, next_image
      A(:)[next_image, 1, 1] = A(:)[image, 1, 1]
      print *,"PRINTING: ",image,":",A(:)[image, 1, 1]
      print *,"PRINTING: ",next_image,":",A(:)[next_image, 1, 1]
   end subroutine copyData

   subroutine computeDataInNext(A, results)
      complex, dimension(:), intent(in) :: A
      complex, dimension(:), intent(out) :: results
      results = sin(A)
   end subroutine computeDataInNext

   subroutine verifyComputation(A, results, image)
      complex, dimension(:), intent(in) :: A
      complex, dimension(:), intent(in) :: results
      integer, intent(in) :: image
      do i = 1, SIZE(A)
         if (sin(A(i)) .NE. results(i)) then
            print *,sin(A(i))," .NE. ",results(i)
            ERROR STOP 101
         end if
      end do
      print *,"VERIFIED: ",image,":",sin(A)
      print *,"VERIFIED: ",image,":",results
   end subroutine verifyComputation

end module asc

program main

   use asc

   implicit none
   integer, parameter :: SIZE = 30
   integer x, images, image, next_image

   complex, dimension(SIZE), save :: A[1, 1, *]
   complex, dimension(SIZE), save :: results[1, 1, *]

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
         A(x)[image, 1, 1] = ( mod(x+image*next_image, SIZE) + 1.2, mod(x+image*next_image, SIZE) + 0.2 )
      end do

      call copyData(A, image, next_image)
      call computeDataInNext(A(:)[next_image, 1, 1], results(:)[image, 1, 1])

      ! Verify the computation
      call verifyComputation(A(:)[image, 1, 1], results(:)[image, 1, 1], image);
   end if

   SYNC ALL

end program main