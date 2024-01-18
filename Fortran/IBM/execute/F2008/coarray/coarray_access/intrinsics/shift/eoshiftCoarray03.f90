!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : eoshiftCoarray01.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2010-12-16
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF Array Intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : EOSHIFT
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
!*  This program tests the EOSHIFT intrinsic procedure
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   integer, parameter :: SIZE = 30
   integer row, col, z

   integer B_I
   integer, dimension(SIZE), save, codimension[*] :: M_I
   integer, dimension(SIZE, SIZE, SIZE), save, codimension[*] :: X_I

   real B_R
   real, dimension(SIZE), save, codimension[*] :: M_R
   real, dimension(SIZE, SIZE, SIZE), save, codimension[*] :: X_R

   complex B_C
   complex, dimension(SIZE), save, codimension[*] :: M_C
   complex, dimension(SIZE, SIZE, SIZE), save, codimension[*] :: X_C

   !
   ! Initialization of the data using the image
   !
   do row = 1, SIZE
      M_I(row) = row + this_image()
      M_R(row) = row * 1.5 + this_image()
      M_C(row) = row * (1.5, 0.5) + this_image()
   end do
   do row = 1, SIZE
      do col = 1, SIZE
         do z = 1, SIZE
            X_I(z, col, row) = this_image()
            X_R(row, col, z) = this_image() * 1.5
            X_C(row, col, z) = this_image() * (1.5, 0.5)
         end do
      end do
   end do

   !
   ! Test 1: integer type
   !
   B_I = -this_image()
   M_I = eoshift(M_I, SHIFT=SIZE, BOUNDARY=B_I, DIM=1)
   do row = 1, SIZE
      if (M_I(row) .NE. B_I) ERROR STOP 101
   end do

   X_I = eoshift(X_I, SHIFT=SIZE, BOUNDARY=B_I, DIM=3)
   do row = 1, SIZE
      do col = 1, SIZE
         do z = 1, SIZE
            if (X_I(z,col,row) .NE. B_I) ERROR STOP 103
         end do
      end do
   end do

   !
   ! Test 2: real type
   !
   B_R = -this_image()*1.5
   M_R = eoshift(M_R, SHIFT=SIZE, BOUNDARY=B_R, DIM=1)
   do row = 1, SIZE
      if (M_R(row) .NE. B_R) ERROR STOP 201
   end do

   X_R = eoshift(X_R, SHIFT=SIZE, BOUNDARY=B_R, DIM=3)
   do row = 1, SIZE
      do col = 1, SIZE
         do z = 1, SIZE
            if (X_R(z,col,row) .NE. B_R) ERROR STOP 203
         end do
      end do
   end do

   !
   ! Test 3: complex type
   !
   B_C = -this_image()*(1.5,0.5)
   M_C = eoshift(M_C, SHIFT=SIZE, BOUNDARY=B_C, DIM=1)
   do row = 1, SIZE
      if (M_C(row) .NE. B_C) ERROR STOP 301
   end do

   X_C = eoshift(X_C, SHIFT=SIZE, BOUNDARY=B_C, DIM=3)
   do row = 1, SIZE
      do col = 1, SIZE
         do z = 1, SIZE
            if (X_C(z,col,row) .NE. B_C) ERROR STOP 303
         end do
      end do
   end do

end program main
