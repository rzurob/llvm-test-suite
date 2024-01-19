!*  ============================================================================
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
!*
!*  CSHIFT the Matrices and then compare with the already shifted matrices
!*  This test uses different array ranks and co-array ranks
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

   integer i, j
   integer, parameter :: SIZE = 10

   integer, dimension(SIZE), save, codimension[SIZE, *] :: M_I
   integer, dimension(SIZE) :: M_I_R
   integer, dimension(SIZE/2, SIZE/2), save, codimension[SIZE/2, SIZE/2, *] :: X_I
   integer, dimension(SIZE/2, SIZE/2) :: X_I_R

   real, dimension(SIZE), save, codimension[SIZE/2, *] :: M_R
   real, dimension(SIZE) :: M_R_R
   real, dimension(SIZE/2, SIZE/2), save, codimension[SIZE/2, SIZE/2, *] :: X_R
   real, dimension(SIZE/2, SIZE/2) :: X_R_R

   complex, dimension(SIZE), save, codimension[SIZE/4, *] :: M_C
   complex, dimension(SIZE) :: M_C_R
   complex, dimension(SIZE/2, SIZE/2), save, codimension[SIZE/2, SIZE/2, *] :: X_C
   complex, dimension(SIZE/2, SIZE/2) :: X_C_R

   M_I = (/ 1,2,3,4,5,6,7,8,9,10 /)
   M_I_R = (/ 6,7,8,9,10,1,2,3,4,5 /)
   X_I = reshape ( (/ 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25 /), shape(X_I) )
   X_I_R = reshape ( (/ 11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,1,2,3,4,5,6,7,8,9,10 /), shape(X_I_R) )

   !
   ! Test 1: integer type
   !
   M_I = cshift(M_I, SIZE/2, 1)
   do i = 1, SIZE
      if (M_I(i) .NE. M_I_R(i)) then
         print *,M_I(i)," .NE. ",M_I_R(i)
         ERROR STOP 101
      end if
   end do
   X_I = cshift(X_I, SIZE/4, 2)
   do i = 1, SIZE/2
      do j = 1, SIZE/2
         if (X_I(i,j) .NE. X_I_R(i,j)) then
            print *,X_I(i,j)," .NE. ",X_I_R(i,j)
            ERROR STOP 103
         end if
      end do
   end do

   !
   ! Test 1: real type
   !
   M_R = (/ 1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5 /)
   M_R_R = (/ 3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,1.5,2.5 /)
   X_R = reshape ( (/ 1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5,17.5,18.5,19.5,20.5,21.5,22.5,23.5,24.5,25.5 /), shape(X_R) )
   X_R_R = reshape ( (/ 6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,15.5,16.5,17.5,18.5,19.5,20.5,21.5,22.5,23.5,24.5,25.5,1.5,2.5,3.5,4.5,5.5 /), shape(X_R_R) )

   M_R = cshift(M_R, SIZE/4, 1)
   do i = 1, SIZE
      if (M_R(i) .NE. M_R_R(i)) then
         print *,M_R(i)," .NE. ",M_R_R(i)
         ERROR STOP 201
      end if
   end do
   X_R = cshift(X_R, SIZE/8, 2)
   do i = 1, SIZE/2
      do j = 1, SIZE/2
         if (X_R(i,j) .NE. X_R_R(i,j)) then
            print *,X_R(i,j)," .NE. ",X_R_R(i,j)
            ERROR STOP 203
         end if
      end do
   end do

   M_C = (/ (1.5,0.5),(2.5,0.5),(3.5,0.5),(4.5,0.5),(5.5,0.5),(6.5,0.5),(7.5,0.5),(8.5,0.5),(9.5,0.5),(10.5,0.5) /)
   M_C_R = (/ (3.5,0.5),(4.5,0.5),(5.5,0.5),(6.5,0.5),(7.5,0.5),(8.5,0.5),(9.5,0.5),(10.5,0.5),(1.5,0.5),(2.5,0.5) /)
   X_C = reshape ( (/ (1.5,0.5),(2.5,0.5),(3.5,0.5),(4.5,0.5),(5.5,0.5),(6.5,0.5),(7.5,0.5),(8.5,0.5),(9.5,0.5),(10.5,0.5),(11.5,0.5),(12.5,0.5),(13.5,0.5),(14.5,0.5),(15.5,0.5),(16.5,0.5),(17.5,0.5),(18.5,0.5),(19.5,0.5),(20.5,0.5),(21.5,0.5),(22.5,0.5),(23.5,0.5),(24.5,0.5),(25.5,0.5) /), shape(X_C) )
   X_C_R = reshape ( (/ (6.5,0.5),(7.5,0.5),(8.5,0.5),(9.5,0.5),(10.5,0.5),(11.5,0.5),(12.5,0.5),(13.5,0.5),(14.5,0.5),(15.5,0.5),(16.5,0.5),(17.5,0.5),(18.5,0.5),(19.5,0.5),(20.5,0.5),(21.5,0.5),(22.5,0.5),(23.5,0.5),(24.5,0.5),(25.5,0.5),(1.5,0.5),(2.5,0.5),(3.5,0.5),(4.5,0.5),(5.5,0.5) /), shape(X_C_R) )

   !
   ! Test 1: complex type
   !
   M_C = cshift(M_C, SIZE/4, 1)
   do i = 1, SIZE
      if (M_C(i) .NE. M_C_R(i)) then
         print *,M_C(i)," .NE. ",M_C_R(i)
         ERROR STOP 301
      end if
   end do
   X_C = cshift(X_C, SIZE/8, 2)
   do i = 1, SIZE/2
      do j = 1, SIZE/2
         if (X_C(i,j) .NE. X_C_R(i,j)) then
            print *,X_C(i,j)," .NE. ",X_C_R(i,j)
            ERROR STOP 303
         end if
      end do
   end do

end program main
