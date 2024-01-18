!*  ============================================================================
!*
!*  TEST CASE NAME             : cshiftCoarray01.f
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
!*  This program tests the CSHIFT intrinsic procedure
!*  The origiinal stored value, which is not changed, is compared
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   integer, parameter :: SIZE = 30
   integer row, col, z, value

   integer, dimension(SIZE), save, codimension[*] :: M_I, S_I
   integer, dimension(SIZE, SIZE, SIZE), save, codimension[*] :: X_I, Y_I

   real, dimension(SIZE), save, codimension[*] :: M_R, S_R
   real, dimension(SIZE, SIZE, SIZE), save, codimension[*] :: X_R, Y_R

   complex, dimension(SIZE), save, codimension[*] :: M_C, S_C
   complex, dimension(SIZE, SIZE, SIZE), save, codimension[*] :: X_C, Y_C

   !
   ! Initialization of the data
   !
   if (this_image() .EQ. 1) then
      do row = 1, SIZE
         M_I(row) = row
         M_R(row) = row * 1.5
         M_C(row) = row * (1.5, 0.5)
      end do
      value = 1
      do row = 1, SIZE
         do col = 1, SIZE
            do z = 1, SIZE
               X_I(z, col, row) = value
               X_R(row, col, z) = value * 1.5
               X_C(row, col, z) = value * (1.5, 0.5)
               value = value + 1
            end do
         end do
      end do
      S_I = M_I
      S_R = M_R
      S_C = M_C
      Y_I = X_I
      Y_R = X_R
      Y_C = X_C
   end if
   sync all
   if (this_image() .NE. 1) then
      M_I = M_I[1] + this_image()
      M_R = M_R[1] + this_image()
      M_C = M_C[1] + this_image()
      X_I = X_I[1] + this_image()
      X_R = X_R[1] + this_image()
      X_C = X_C[1] + this_image()
   end if

   !
   ! CSHIFT the local Matrices SIZE times and check if it's
   ! equal to the Matrices in image 1. The matrix in image 1
   ! is first stored then cshifted SIZE times and compared
   ! with the stored matrix.
   !
   ! Test 1: integer type
   !
   M_I = cshift(M_I, SIZE, 1)
   if (this_image() .EQ. 1) then
      do row = 1, SIZE
         if (M_I(row) .NE. S_I(row)) ERROR STOP 101
      end do
   else
      M_I = M_I - this_image()
      do row = 1, SIZE
         if (M_I(row) .NE. S_I(row)[1]) ERROR STOP 103
      end do
   end if

   X_I = cshift(X_I, SIZE, 3)
   if (this_image() .EQ. 1) then
      do row = 1, SIZE
         do col = 1, SIZE
            do z = 1, SIZE
               if (X_I(z,col,row) .NE. Y_I(z,col,row)) ERROR STOP 105
            end do
         end do
      end do
   else
      X_I = X_I - this_image()
      do row = 1, SIZE
         do col = 1, SIZE
            do z = 1, SIZE
               if (X_I(z,col,row) .NE. Y_I(z,col,row)[1]) ERROR STOP 107
            end do
         end do
      end do
   end if

   !
   ! Test 2: real type
   !
   M_R = cshift(M_R, SIZE, 1)
   if (this_image() .EQ. 1) then
      do row = 1, SIZE
         if (M_R(row) .NE. S_R(row)) ERROR STOP 201
      end do
   else
      M_R = M_R - this_image()
      do row = 1, SIZE
         if (M_R(row) .NE. S_R(row)[1]) ERROR STOP 203
      end do
   end if

   X_R = cshift(X_R, SIZE, 3)
   if (this_image() .EQ. 1) then
      do row = 1, SIZE
         do col = 1, SIZE
            do z = 1, SIZE
               if (X_R(z,col,row) .NE. Y_R(z,col,row)) ERROR STOP 205
            end do
         end do
      end do
   else
      X_R = X_R - this_image()
      do row = 1, SIZE
         do col = 1, SIZE
            do z = 1, SIZE
               if (X_R(z,col,row) .NE. Y_R(z,col,row)[1]) ERROR STOP 207
            end do
         end do
      end do
   end if

   !
   ! Test 2: complex type
   !
   M_C = cshift(M_C, SIZE, 1)
   if (this_image() .EQ. 1) then
      do row = 1, SIZE
         if (M_C(row) .NE. S_C(row)) ERROR STOP 301
      end do
   else
      M_C = M_C - this_image()
      do row = 1, SIZE
         if (M_C(row) .NE. S_C(row)[1]) ERROR STOP 303
      end do
   end if

   X_C = cshift(X_C, SIZE, 3)
   if (this_image() .EQ. 1) then
      do row = 1, SIZE
         do col = 1, SIZE
            do z = 1, SIZE
               if (X_C(z,col,row) .NE. Y_C(z,col,row)) ERROR STOP 305
            end do
         end do
      end do
   else
      X_C = X_C - this_image()
      do row = 1, SIZE
         do col = 1, SIZE
            do z = 1, SIZE
               if (X_C(z,col,row) .NE. Y_C(z,col,row)[1]) ERROR STOP 307
            end do
         end do
      end do
   end if

end program main
