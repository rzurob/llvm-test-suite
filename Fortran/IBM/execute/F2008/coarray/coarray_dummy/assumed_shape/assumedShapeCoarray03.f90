!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : assumedShapeCoarray03.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2011-02-10
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
!*  Arguments passed to control the shape (lower_bound=SIZE/2 and upper_bound=SIZE)
!*  with corank 0. Type used logical, character, integer, real and complex
!*
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

   logical, dimension(SIZE), save :: L_1[*]
   logical, dimension(SIZE, SIZE), save :: L_2[*]
   logical, dimension(SIZE, SIZE, SIZE), save :: L_3[*]

   character, dimension(SIZE), save :: C_1[*]
   character, dimension(SIZE, SIZE), save :: C_2[*]
   character, dimension(SIZE, SIZE, SIZE), save :: C_3[*]

   integer, dimension(SIZE), save :: I_1[*]
   integer, dimension(SIZE, SIZE), save :: I_2[*]
   integer, dimension(SIZE, SIZE, SIZE), save :: I_3[*]

   real, dimension(SIZE), save :: R_1[*]
   real, dimension(SIZE, SIZE), save :: R_2[*]
   real, dimension(SIZE, SIZE, SIZE), save :: R_3[*]

   complex, dimension(SIZE), save :: COMPLEX_1[*]
   complex, dimension(SIZE, SIZE), save :: COMPLEX_2[*]
   complex, dimension(SIZE, SIZE, SIZE), save :: COMPLEX_3[*]

   !
   ! Initialization of the data
   !
   A = iachar('A') - num_images()
   do x = SIZE/2+1, SIZE
      if ((x-(SIZE/2)) .LE. num_images()) then
         L_1(x) = .TRUE.
      end if
      C_1(x) = achar(A + (x-(SIZE/2)) + num_images())
      I_1(x) = (x-(SIZE/2)) + num_images()
      R_1(x) = (x-(SIZE/2)) + num_images() * 1.5
      COMPLEX_1(x) = ((x-(SIZE/2)) + num_images() * 1.5, (x-(SIZE/2)) + num_images() * 0.5)
   end do
   do x = SIZE/2+1, SIZE
      do y = SIZE/2+1, SIZE
         if ((x-(SIZE/2)) + (y-(SIZE/2)) .LE. num_images()) then
            L_2(x, y) = .TRUE.
         end if
         C_2(x, y) = achar(A + (x-(SIZE/2)) + (y-(SIZE/2)) + num_images())
         I_2(x, y) = (x-(SIZE/2)) + (y-(SIZE/2)) + num_images()
         R_2(x, y) = (x-(SIZE/2)) + (y-(SIZE/2)) + num_images() * 2.5
         COMPLEX_2(x, y) = ((x-(SIZE/2)) + (y-(SIZE/2)) + num_images() * 2.5, (x-(SIZE/2)) + (y-(SIZE/2)) + num_images() * 1.5)
      end do
   end do
   do x = SIZE/2+1, SIZE
      do y = SIZE/2+1, SIZE
         do z = SIZE/2+1, SIZE
            if ((x-(SIZE/2)) + (y-(SIZE/2)) + (z-(SIZE/2)) .LE. num_images()) then
               L_3(x, y, z) = .TRUE.
            end if
            C_3(x, y, z) = achar(A + (x-(SIZE/2)) + (y-(SIZE/2)) + (z-(SIZE/2)) + num_images())
            I_3(x, y, z) = (x-(SIZE/2)) + (y-(SIZE/2)) + (z-(SIZE/2)) + num_images()
            R_3(x, y, z) = (x-(SIZE/2)) + (y-(SIZE/2)) + (z-(SIZE/2)) + num_images() * 3.5
            COMPLEX_3(x, y, z) = ((x-(SIZE/2)) + (y-(SIZE/2)) + (z-(SIZE/2)) + num_images() * 3.5, (x-(SIZE/2)) + (y-(SIZE/2)) + (z-(SIZE/2)) + num_images() * 2.5)
         end do
      end do
   end do

   call asc_L_1(L_1(SIZE/2+1:SIZE))
   call asc_L_2(L_2(SIZE/2+1:SIZE, SIZE/2+1:SIZE), SIZE/2)
   call asc_L_3(L_3(SIZE/2+1:SIZE, SIZE/2+1:SIZE, SIZE/2+1:SIZE), SIZE/2)

   call asc_C_1(C_1(SIZE/2+1:SIZE))
   call asc_C_2(C_2(SIZE/2+1:SIZE, SIZE/2+1:SIZE), SIZE/2)
   call asc_C_3(C_3(SIZE/2+1:SIZE, SIZE/2+1:SIZE, SIZE/2+1:SIZE), SIZE/2)

   call asc_I_1(I_1(SIZE/2+1:SIZE))
   call asc_I_2(I_2(SIZE/2+1:SIZE, SIZE/2+1:SIZE), SIZE/2)
   call asc_I_3(I_3(SIZE/2+1:SIZE, SIZE/2+1:SIZE, SIZE/2+1:SIZE), SIZE/2)

   call asc_R_1(R_1(SIZE/2+1:SIZE))
   call asc_R_2(R_2(SIZE/2+1:SIZE, SIZE/2+1:SIZE), SIZE/2)
   call asc_R_3(R_3(SIZE/2+1:SIZE, SIZE/2+1:SIZE, SIZE/2+1:SIZE), SIZE/2)

   call asc_COMPLEX_1(COMPLEX_1(SIZE/2+1:SIZE))
   call asc_COMPLEX_2(COMPLEX_2(SIZE/2+1:SIZE, SIZE/2+1:SIZE), SIZE/2)
   call asc_COMPLEX_3(COMPLEX_3(SIZE/2+1:SIZE, SIZE/2+1:SIZE, SIZE/2+1:SIZE), SIZE/2)

end program main
