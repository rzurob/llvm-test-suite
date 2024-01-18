!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : cshiftCoarray04.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2010-12-16
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF Array Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : CSHIFT
!*
!*  REFERENCE                  : Feature Number 385143
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
!*  This program tests the CSHIFT intrinsic procedure
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   integer row
   integer, parameter :: SIZE = 30
   complex, dimension(SIZE), save, codimension[*] :: M_C, S_C

   !
   ! First it initializes the data to 0 in all images.
   ! Then use image 1 to initializes the data.
   ! This data is then use by all other images to initialize
   ! there data.
   ! SYNC IMAGES(1) is used to make sure that the data is first
   ! initialized by image 1.
   ! After cshifting the data its compared with the knwon value
   ! stored in image 1
   !
   M_C = (0.0, 0.0)
   S_C = (1.0, 1.0)
   ! For verification that the data has been initialized to 0
   print *,this_image(),": ",M_C
   if (this_image() .NE. 1) then
      SYNC IMAGES(1)
      M_C = M_C[1]
      M_C = cshift(M_C, SIZE, 1)
      ! For verification that the data has been initialized and shifted
      print *,this_image(),": ",M_C
      do row = 1, SIZE
         if (M_C(row) .NE. S_C(row)[1]) ERROR STOP 103
      end do
   else
      do row = 1, SIZE
         M_C(row) = row * (1.5, 0.5)
      end do
      S_C = M_C
      M_C = cshift(M_C, SIZE, 1)
      ! For verification that the data has been initialized and shifted
      ! Only this data should be verified in the output to make
      ! the test independent of number of images
      print *,this_image(),": ",M_C
      do row = 1, SIZE
         if (M_C(row) .NE. S_C(row)) ERROR STOP 101
      end do
      SYNC IMAGES(*)
   end if

end program main
