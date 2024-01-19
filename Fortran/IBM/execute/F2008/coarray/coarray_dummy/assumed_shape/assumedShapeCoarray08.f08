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
!*  Arguments passed to control the shape (lower_bound=SIZE/4 and upper_bound=SIZE-SIZE/4)
!*  with different coranks. Type used logical, character, integer, real and complex
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   use asc_L
   use asc_C
   use asc_I
   use asc_R
   use asc_COMPLEX

   implicit none
   integer, parameter :: SIZE = 8
   integer A, x, y, z

   logical, dimension(SIZE), save :: L_1[1, *]
   logical, dimension(SIZE), save :: L_1_2[1, 1, *]
   logical, dimension(SIZE), save :: L_1_3[1, 1, *]
   logical, dimension(SIZE), save :: L_1_4[1, 1, 1, *]
   logical, dimension(SIZE), save :: L_1_5[1, 1, 1, *]
   logical, dimension(SIZE), save :: L_1_6[1, 1, 1, *]
   logical, dimension(SIZE, SIZE), save :: L_2[1, *]
   logical, dimension(SIZE, SIZE), save :: L_2_2[1, 1, *]
   logical, dimension(SIZE, SIZE), save :: L_2_3[1, 1, *]
   logical, dimension(SIZE, SIZE), save :: L_2_4[1, 1, 1, *]
   logical, dimension(SIZE, SIZE), save :: L_2_5[1, 1, 1, *]
   logical, dimension(SIZE, SIZE), save :: L_2_6[1, 1, 1, *]
   logical, dimension(SIZE, SIZE, SIZE), save :: L_3[1, *]
   logical, dimension(SIZE, SIZE, SIZE), save :: L_3_2[1, 1, *]
   logical, dimension(SIZE, SIZE, SIZE), save :: L_3_3[1, 1, *]
   logical, dimension(SIZE, SIZE, SIZE), save :: L_3_4[1, 1, 1, *]
   logical, dimension(SIZE, SIZE, SIZE), save :: L_3_5[1, 1, 1, *]
   logical, dimension(SIZE, SIZE, SIZE), save :: L_3_6[1, 1, 1, *]

   character, dimension(SIZE), save :: C_1[1, *]
   character, dimension(SIZE), save :: C_1_2[1, 1, *]
   character, dimension(SIZE), save :: C_1_3[1, 1, *]
   character, dimension(SIZE), save :: C_1_4[1, 1, 1, *]
   character, dimension(SIZE), save :: C_1_5[1, 1, 1, *]
   character, dimension(SIZE), save :: C_1_6[1, 1, 1, *]
   character, dimension(SIZE, SIZE), save :: C_2[1, *]
   character, dimension(SIZE, SIZE), save :: C_2_2[1, 1, *]
   character, dimension(SIZE, SIZE), save :: C_2_3[1, 1, *]
   character, dimension(SIZE, SIZE), save :: C_2_4[1, 1, 1, *]
   character, dimension(SIZE, SIZE), save :: C_2_5[1, 1, 1, *]
   character, dimension(SIZE, SIZE), save :: C_2_6[1, 1, 1, *]
   character, dimension(SIZE, SIZE, SIZE), save :: C_3[1, *]
   character, dimension(SIZE, SIZE, SIZE), save :: C_3_2[1, 1, *]
   character, dimension(SIZE, SIZE, SIZE), save :: C_3_3[1, 1, *]
   character, dimension(SIZE, SIZE, SIZE), save :: C_3_4[1, 1, 1, *]
   character, dimension(SIZE, SIZE, SIZE), save :: C_3_5[1, 1, 1, *]
   character, dimension(SIZE, SIZE, SIZE), save :: C_3_6[1, 1, 1, *]

   integer, dimension(SIZE), save :: I_1[1, *]
   integer, dimension(SIZE), save :: I_1_2[1, 1, *]
   integer, dimension(SIZE), save :: I_1_3[1, 1, *]
   integer, dimension(SIZE), save :: I_1_4[1, 1, 1, *]
   integer, dimension(SIZE), save :: I_1_5[1, 1, 1, *]
   integer, dimension(SIZE), save :: I_1_6[1, 1, 1, *]
   integer, dimension(SIZE, SIZE), save :: I_2[1, *]
   integer, dimension(SIZE, SIZE), save :: I_2_2[1, 1, *]
   integer, dimension(SIZE, SIZE), save :: I_2_3[1, 1, *]
   integer, dimension(SIZE, SIZE), save :: I_2_4[1, 1, 1, *]
   integer, dimension(SIZE, SIZE), save :: I_2_5[1, 1, 1, *]
   integer, dimension(SIZE, SIZE), save :: I_2_6[1, 1, 1, *]
   integer, dimension(SIZE, SIZE, SIZE), save :: I_3[1, *]
   integer, dimension(SIZE, SIZE, SIZE), save :: I_3_2[1, 1, *]
   integer, dimension(SIZE, SIZE, SIZE), save :: I_3_3[1, 1, *]
   integer, dimension(SIZE, SIZE, SIZE), save :: I_3_4[1, 1, 1, *]
   integer, dimension(SIZE, SIZE, SIZE), save :: I_3_5[1, 1, 1, *]
   integer, dimension(SIZE, SIZE, SIZE), save :: I_3_6[1, 1, 1, *]

   real, dimension(SIZE), save :: R_1[1, *]
   real, dimension(SIZE), save :: R_1_2[1, 1, *]
   real, dimension(SIZE), save :: R_1_3[1, 1, *]
   real, dimension(SIZE), save :: R_1_4[1, 1, 1, *]
   real, dimension(SIZE), save :: R_1_5[1, 1, 1, *]
   real, dimension(SIZE), save :: R_1_6[1, 1, 1, *]
   real, dimension(SIZE, SIZE), save :: R_2[1, *]
   real, dimension(SIZE, SIZE), save :: R_2_2[1, 1, *]
   real, dimension(SIZE, SIZE), save :: R_2_3[1, 1, *]
   real, dimension(SIZE, SIZE), save :: R_2_4[1, 1, 1, *]
   real, dimension(SIZE, SIZE), save :: R_2_5[1, 1, 1, *]
   real, dimension(SIZE, SIZE), save :: R_2_6[1, 1, 1, *]
   real, dimension(SIZE, SIZE, SIZE), save :: R_3[1, *]
   real, dimension(SIZE, SIZE, SIZE), save :: R_3_2[1, 1, *]
   real, dimension(SIZE, SIZE, SIZE), save :: R_3_3[1, 1, *]
   real, dimension(SIZE, SIZE, SIZE), save :: R_3_4[1, 1, 1, *]
   real, dimension(SIZE, SIZE, SIZE), save :: R_3_5[1, 1, 1, *]
   real, dimension(SIZE, SIZE, SIZE), save :: R_3_6[1, 1, 1, *]

   complex, dimension(SIZE), save :: COMPLEX_1[1, *]
   complex, dimension(SIZE), save :: COMPLEX_1_2[1, 1, *]
   complex, dimension(SIZE), save :: COMPLEX_1_3[1, 1, *]
   complex, dimension(SIZE), save :: COMPLEX_1_4[1, 1, 1, *]
   complex, dimension(SIZE), save :: COMPLEX_1_5[1, 1, 1, *]
   complex, dimension(SIZE), save :: COMPLEX_1_6[1, 1, 1, *]
   complex, dimension(SIZE, SIZE), save :: COMPLEX_2[1, *]
   complex, dimension(SIZE, SIZE), save :: COMPLEX_2_2[1, 1, *]
   complex, dimension(SIZE, SIZE), save :: COMPLEX_2_3[1, 1, *]
   complex, dimension(SIZE, SIZE), save :: COMPLEX_2_4[1, 1, 1, *]
   complex, dimension(SIZE, SIZE), save :: COMPLEX_2_5[1, 1, 1, *]
   complex, dimension(SIZE, SIZE), save :: COMPLEX_2_6[1, 1, 1, *]
   complex, dimension(SIZE, SIZE, SIZE), save :: COMPLEX_3[1, *]
   complex, dimension(SIZE, SIZE, SIZE), save :: COMPLEX_3_2[1, 1, *]
   complex, dimension(SIZE, SIZE, SIZE), save :: COMPLEX_3_3[1, 1, *]
   complex, dimension(SIZE, SIZE, SIZE), save :: COMPLEX_3_4[1, 1, 1, *]
   complex, dimension(SIZE, SIZE, SIZE), save :: COMPLEX_3_5[1, 1, 1, *]
   complex, dimension(SIZE, SIZE, SIZE), save :: COMPLEX_3_6[1, 1, 1, *]


   !
   ! Initialization of the data
   !
   A = iachar('A') - num_images()
   do x = SIZE/4+1, SIZE-SIZE/4
      if ((x-(SIZE/4)) .LE. num_images()) then
         L_1(x)[this_image(), 1] = .TRUE.
         L_1_2(x)[this_image(), 1, 1] = .TRUE.
         L_1_3(x)[1, this_image(), 1] = .TRUE.
         L_1_4(x)[this_image(), 1, 1, 1] = .TRUE.
         L_1_5(x)[1, this_image(), 1, 1] = .TRUE.
         L_1_6(x)[1, 1, this_image(), 1] = .TRUE.
      end if
      C_1(x)[this_image(), 1] = achar(A + (x-(SIZE/4)) + num_images())
      C_1_2(x)[this_image(), 1, 1] = achar(A + (x-(SIZE/4)) + num_images())
      C_1_3(x)[1, this_image(), 1] = achar(A + (x-(SIZE/4)) + num_images())
      C_1_4(x)[this_image(), 1, 1, 1] = achar(A + (x-(SIZE/4)) + num_images())
      C_1_5(x)[1, this_image(), 1, 1] = achar(A + (x-(SIZE/4)) + num_images())
      C_1_6(x)[1, 1, this_image(), 1] = achar(A + (x-(SIZE/4)) + num_images())
      I_1(x)[this_image(), 1] = (x-(SIZE/4)) + num_images()
      I_1_2(x)[this_image(), 1, 1] = (x-(SIZE/4)) + num_images()
      I_1_3(x)[1, this_image(), 1] = (x-(SIZE/4)) + num_images()
      I_1_4(x)[this_image(), 1, 1, 1] = (x-(SIZE/4)) + num_images()
      I_1_5(x)[1, this_image(), 1, 1] = (x-(SIZE/4)) + num_images()
      I_1_6(x)[1, 1, this_image(), 1] = (x-(SIZE/4)) + num_images()
      R_1(x)[this_image(), 1] = (x-(SIZE/4)) + num_images() * 1.5
      R_1_2(x)[this_image(), 1, 1] = (x-(SIZE/4)) + num_images() * 1.5
      R_1_3(x)[1, this_image(), 1] = (x-(SIZE/4)) + num_images() * 1.5
      R_1_4(x)[this_image(), 1, 1, 1] = (x-(SIZE/4)) + num_images() * 1.5
      R_1_5(x)[1, this_image(), 1, 1] = (x-(SIZE/4)) + num_images() * 1.5
      R_1_6(x)[1, 1, this_image(), 1] = (x-(SIZE/4)) + num_images() * 1.5
      COMPLEX_1(x) = ((x-(SIZE/4)) + num_images() * 1.5, (x-(SIZE/4)) + num_images() * 0.5)
      COMPLEX_1(x)[this_image(), 1] = ((x-(SIZE/4)) + num_images() * 1.5, (x-(SIZE/4)) + num_images() * 0.5)
      COMPLEX_1_2(x)[this_image(), 1, 1] = ((x-(SIZE/4)) + num_images() * 1.5, (x-(SIZE/4)) + num_images() * 0.5)
      COMPLEX_1_3(x)[1, this_image(), 1] = ((x-(SIZE/4)) + num_images() * 1.5, (x-(SIZE/4)) + num_images() * 0.5)
      COMPLEX_1_4(x)[this_image(), 1, 1, 1] = ((x-(SIZE/4)) + num_images() * 1.5, (x-(SIZE/4)) + num_images() * 0.5)
      COMPLEX_1_5(x)[1, this_image(), 1, 1] = ((x-(SIZE/4)) + num_images() * 1.5, (x-(SIZE/4)) + num_images() * 0.5)
      COMPLEX_1_6(x)[1, 1, this_image(), 1] = ((x-(SIZE/4)) + num_images() * 1.5, (x-(SIZE/4)) + num_images() * 0.5)
   end do
   do x = SIZE/4+1, SIZE-SIZE/4
      do y = SIZE/4+1, SIZE-SIZE/4
         if ((x-(SIZE/4)) + (y-(SIZE/4)) .LE. num_images()) then
            L_2(x, y)[this_image(), 1] = .TRUE.
            L_2_2(x, y)[this_image(), 1, 1] = .TRUE.
            L_2_3(x, y)[1, this_image(), 1] = .TRUE.
            L_2_4(x, y)[this_image(), 1, 1, 1] = .TRUE.
            L_2_5(x, y)[1, this_image(), 1, 1] = .TRUE.
            L_2_6(x, y)[1, 1, this_image(), 1] = .TRUE.
         end if
         C_2(x, y)[this_image(), 1] = achar(A + (x-(SIZE/4)) + (y-(SIZE/4)) + num_images())
         C_2_2(x, y)[this_image(), 1, 1] = achar(A + (x-(SIZE/4)) + (y-(SIZE/4)) + num_images())
         C_2_3(x, y)[1, this_image(), 1] = achar(A + (x-(SIZE/4)) + (y-(SIZE/4)) + num_images())
         C_2_4(x, y)[this_image(), 1, 1, 1] = achar(A + (x-(SIZE/4)) + (y-(SIZE/4)) + num_images())
         C_2_5(x, y)[1, this_image(), 1, 1] = achar(A + (x-(SIZE/4)) + (y-(SIZE/4)) + num_images())
         C_2_6(x, y)[1, 1, this_image(), 1] = achar(A + (x-(SIZE/4)) + (y-(SIZE/4)) + num_images())
         I_2(x, y)[this_image(), 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + num_images()
         I_2_2(x, y)[this_image(), 1, 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + num_images()
         I_2_3(x, y)[1, this_image(), 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + num_images()
         I_2_4(x, y)[this_image(), 1, 1, 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + num_images()
         I_2_5(x, y)[1, this_image(), 1, 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + num_images()
         I_2_6(x, y)[1, 1, this_image(), 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + num_images()
         R_2(x, y)[this_image(), 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 2.5
         R_2_2(x, y)[this_image(), 1, 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 2.5
         R_2_3(x, y)[1, this_image(), 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 2.5
         R_2_4(x, y)[this_image(), 1, 1, 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 2.5
         R_2_5(x, y)[1, this_image(), 1, 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 2.5
         R_2_6(x, y)[1, 1, this_image(), 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 2.5
         COMPLEX_2(x, y) = ((x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 1.5, (x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 0.5)
         COMPLEX_2(x, y)[this_image(), 1] = ((x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 2.5, (x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 1.5)
         COMPLEX_2_2(x, y)[this_image(), 1, 1] = ((x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 2.5, (x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 1.5)
         COMPLEX_2_3(x, y)[1, this_image(), 1] = ((x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 2.5, (x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 1.5)
         COMPLEX_2_4(x, y)[this_image(), 1, 1, 1] = ((x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 2.5, (x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 1.5)
         COMPLEX_2_5(x, y)[1, this_image(), 1, 1] = ((x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 2.5, (x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 1.5)
         COMPLEX_2_6(x, y)[1, 1, this_image(), 1] = ((x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 2.5, (x-(SIZE/4)) + (y-(SIZE/4)) + num_images() * 1.5)
      end do
   end do
   do x = SIZE/4+1, SIZE-SIZE/4
      do y = SIZE/4+1, SIZE-SIZE/4
         do z = SIZE/4+1, SIZE-SIZE/4
            if ((x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) .LE. num_images()) then
               L_3(x, y, z)[this_image(), 1] = .TRUE.
               L_3_2(x, y, z)[this_image(), 1, 1] = .TRUE.
               L_3_3(x, y, z)[1, this_image(), 1] = .TRUE.
               L_3_4(x, y, z)[this_image(), 1, 1, 1] = .TRUE.
               L_3_5(x, y, z)[1, this_image(), 1, 1] = .TRUE.
               L_3_6(x, y, z)[1, 1, this_image(), 1] = .TRUE.
            end if
            C_3(x, y, z)[this_image(), 1] = achar(A + (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images())
            C_3_2(x, y, z)[this_image(), 1, 1] = achar(A + (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images())
            C_3_3(x, y, z)[1, this_image(), 1] = achar(A + (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images())
            C_3_4(x, y, z)[this_image(), 1, 1, 1] = achar(A + (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images())
            C_3_5(x, y, z)[1, this_image(), 1, 1] = achar(A + (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images())
            C_3_6(x, y, z)[1, 1, this_image(), 1] = achar(A + (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images())
            I_3(x, y, z)[this_image(), 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images()
            I_3_2(x, y, z)[this_image(), 1, 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images()
            I_3_3(x, y, z)[1, this_image(), 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images()
            I_3_4(x, y, z)[this_image(), 1, 1, 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images()
            I_3_5(x, y, z)[1, this_image(), 1, 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images()
            I_3_6(x, y, z)[1, 1, this_image(), 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images()
            R_3(x, y, z)[this_image(), 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 3.5
            R_3_2(x, y, z)[this_image(), 1, 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 3.5
            R_3_3(x, y, z)[1, this_image(), 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 3.5
            R_3_4(x, y, z)[this_image(), 1, 1, 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 3.5
            R_3_5(x, y, z)[1, this_image(), 1, 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 3.5
            R_3_6(x, y, z)[1, 1, this_image(), 1] = (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 3.5
            COMPLEX_3(x, y, z) = ((x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 1.5, (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 2.5)
            COMPLEX_3(x, y, z)[this_image(), 1] = ((x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 3.5, (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 2.5)
            COMPLEX_3_2(x, y, z)[this_image(), 1, 1] = ((x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 3.5, (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 2.5)
            COMPLEX_3_3(x, y, z)[1, this_image(), 1] = ((x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 3.5, (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 2.5)
            COMPLEX_3_4(x, y, z)[this_image(), 1, 1, 1] = ((x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 3.5, (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 2.5)
            COMPLEX_3_5(x, y, z)[1, this_image(), 1, 1] = ((x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 3.5, (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 2.5)
            COMPLEX_3_6(x, y, z)[1, 1, this_image(), 1] = ((x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 3.5, (x-(SIZE/4)) + (y-(SIZE/4)) + (z-(SIZE/4)) + num_images() * 2.5)
         end do
      end do
   end do

   call asc_L_1(L_1(SIZE/4+1:SIZE-SIZE/4))
   call asc_L_1(L_1_2(SIZE/4+1:SIZE-SIZE/4))
   call asc_L_1(L_1_3(SIZE/4+1:SIZE-SIZE/4))
   call asc_L_1(L_1_4(SIZE/4+1:SIZE-SIZE/4))
   call asc_L_1(L_1_5(SIZE/4+1:SIZE-SIZE/4))
   call asc_L_1(L_1_6(SIZE/4+1:SIZE-SIZE/4))
   call asc_L_2(L_2(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_L_2(L_2_2(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_L_2(L_2_3(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_L_2(L_2_4(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_L_2(L_2_5(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_L_2(L_2_6(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_L_3(L_3(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_L_3(L_3_2(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_L_3(L_3_3(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_L_3(L_3_4(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_L_3(L_3_5(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_L_3(L_3_6(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)

   call asc_C_1(C_1(SIZE/4+1:SIZE-SIZE/4))
   call asc_C_1(C_1_2(SIZE/4+1:SIZE-SIZE/4))
   call asc_C_1(C_1_3(SIZE/4+1:SIZE-SIZE/4))
   call asc_C_1(C_1_4(SIZE/4+1:SIZE-SIZE/4))
   call asc_C_1(C_1_5(SIZE/4+1:SIZE-SIZE/4))
   call asc_C_1(C_1_6(SIZE/4+1:SIZE-SIZE/4))
   call asc_C_2(C_2(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_C_2(C_2_2(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_C_2(C_2_3(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_C_2(C_2_4(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_C_2(C_2_5(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_C_2(C_2_6(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_C_3(C_3(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_C_3(C_3_2(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_C_3(C_3_3(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_C_3(C_3_4(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_C_3(C_3_5(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_C_3(C_3_6(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)

   call asc_I_1(I_1(SIZE/4+1:SIZE-SIZE/4))
   call asc_I_1(I_1_2(SIZE/4+1:SIZE-SIZE/4))
   call asc_I_1(I_1_3(SIZE/4+1:SIZE-SIZE/4))
   call asc_I_1(I_1_4(SIZE/4+1:SIZE-SIZE/4))
   call asc_I_1(I_1_5(SIZE/4+1:SIZE-SIZE/4))
   call asc_I_1(I_1_6(SIZE/4+1:SIZE-SIZE/4))
   call asc_I_2(I_2(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_I_2(I_2_2(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_I_2(I_2_3(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_I_2(I_2_4(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_I_2(I_2_5(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_I_2(I_2_6(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_I_3(I_3(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_I_3(I_3_2(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_I_3(I_3_3(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_I_3(I_3_4(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_I_3(I_3_5(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_I_3(I_3_6(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)

   call asc_R_1(R_1(SIZE/4+1:SIZE-SIZE/4))
   call asc_R_1(R_1_2(SIZE/4+1:SIZE-SIZE/4))
   call asc_R_1(R_1_3(SIZE/4+1:SIZE-SIZE/4))
   call asc_R_1(R_1_4(SIZE/4+1:SIZE-SIZE/4))
   call asc_R_1(R_1_5(SIZE/4+1:SIZE-SIZE/4))
   call asc_R_1(R_1_6(SIZE/4+1:SIZE-SIZE/4))
   call asc_R_2(R_2(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_R_2(R_2_2(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_R_2(R_2_3(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_R_2(R_2_4(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_R_2(R_2_5(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_R_2(R_2_6(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_R_3(R_3(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_R_3(R_3_2(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_R_3(R_3_3(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_R_3(R_3_4(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_R_3(R_3_5(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_R_3(R_3_6(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)

   call asc_COMPLEX_1(COMPLEX_1(SIZE/4+1:SIZE-SIZE/4))
   call asc_COMPLEX_1(COMPLEX_1_2(SIZE/4+1:SIZE-SIZE/4))
   call asc_COMPLEX_1(COMPLEX_1_3(SIZE/4+1:SIZE-SIZE/4))
   call asc_COMPLEX_1(COMPLEX_1_4(SIZE/4+1:SIZE-SIZE/4))
   call asc_COMPLEX_1(COMPLEX_1_5(SIZE/4+1:SIZE-SIZE/4))
   call asc_COMPLEX_1(COMPLEX_1_6(SIZE/4+1:SIZE-SIZE/4))
   call asc_COMPLEX_2(COMPLEX_2(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_COMPLEX_2(COMPLEX_2_2(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_COMPLEX_2(COMPLEX_2_3(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_COMPLEX_2(COMPLEX_2_4(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_COMPLEX_2(COMPLEX_2_5(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_COMPLEX_2(COMPLEX_2_6(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_COMPLEX_3(COMPLEX_3(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_COMPLEX_3(COMPLEX_3_2(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_COMPLEX_3(COMPLEX_3_3(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_COMPLEX_3(COMPLEX_3_4(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_COMPLEX_3(COMPLEX_3_5(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)
   call asc_COMPLEX_3(COMPLEX_3_6(SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4, SIZE/4+1:SIZE-SIZE/4), SIZE/4)

end program main
