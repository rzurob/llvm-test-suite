!*  ============================================================================
!*
!*  TEST CASE NAME             : eoshiftCoarray04.f
!*
!*  DATE                       : 2010-12-16
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF Array Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : CSHIFT
!*
!*  REFERENCE                  : Feature Number 385143
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  This program tests the EOSHIFT intrinsic procedure
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   integer row
   integer, parameter :: SIZE = 30
   complex B
   complex, dimension(1), save, codimension[*] :: B_C
   complex, dimension(SIZE), save, codimension[*] :: M_C

   !
   ! First it initializes the data to 0 in all images.
   ! Then use image 1 to initializes the boundary data.
   ! This data is then use by all other images to shift.
   ! SYNC IMAGES(1) is used to make sure that the data is first
   ! initialized by image 1.
   !
   M_C = (1.5, 0.5)
   ! For verification that the data has been initialized
   print *,this_image(),": ",M_C
   B_C = (0.0, 0.0)
   B = (0.0, 0.0)
   print *,this_image(),": ",B_C
   print *,this_image(),": ",B

   if (this_image() .NE. 1) then
      SYNC IMAGES(1)
      B = B_C(1)[1]
      M_C = eoshift(M_C, SHIFT=SIZE, BOUNDARY=B, DIM=1)
      ! For verification that the data has been initialized and shifted
      print *,this_image(),": ",M_C
      do row = 1, SIZE
         if (M_C(row) .NE. B) ERROR STOP 103
      end do
   else
      B_C(1) = (1.75, 0.75)
      M_C = eoshift(M_C, SHIFT=SIZE, BOUNDARY=B_C(1), DIM=1)
      SYNC IMAGES(*)

      ! For verification that the data has been initialized and shifted
      ! Only this data should be verified in the output to make
      ! the test independent of number of images
      print *,this_image(),": ",M_C
      do row = 1, SIZE
         if (M_C(row) .NE. B_C(1)) ERROR STOP 101
      end do
   end if

end program main
