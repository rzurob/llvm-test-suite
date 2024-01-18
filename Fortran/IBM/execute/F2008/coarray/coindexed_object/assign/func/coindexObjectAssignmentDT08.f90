!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectAssignmentDT08.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2011-03-07
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coindex object assignment statement
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type
!*
!*  REFERENCE                  : Feature Number 386924
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
!*  This program tests the coindex object assignment statement of derived type
!*
!*  Uses corank of 2.
!*  Setting data in image 1 for other images. The number of images should be
!*  either less than SIZE or multiple of SIZE. The extra number of images
!*  will be ignored.
!*  Global assignment using intrinsic type complex.
!*
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   ! Sphere type
   type SPHERE
      complex :: x = (-300.0, -300.0)
      complex :: y = (-300.0, -300.0)
      complex :: z = (-300.0, -300.0)
      complex :: radius = (-300.0, -300.0)
      complex :: color = (-300.0, -300.0)
   end type SPHERE

end module DT

module coa

   use DT
   contains

   subroutine verifyImagesProcessed(A, A1, A2, images_processesed, image, SIZE)
      complex, intent(in) :: A(:)[*]
      type(SPHERE), intent(in) :: A1(:)[*]
      type(SPHERE), intent(in) :: A2(:,:)[*]
      integer, intent(in) :: images_processesed, image, SIZE
      integer :: row, col

      if (image .LT. images_processesed) then
         do row = 1, SIZE
            do col = 1, SIZE
               if (A2(row,col)%x .NE. (row+1.5, row+2.5)) then
                  print *,image,":",A2(row,col)%x," .NE. ",(row+1.5, row+2.5)
                  ERROR STOP 11
               else if (A2(row,col)%y .NE. (col+1.5, col+2.5)) then
                  print *,image,":",A2(row,col)%y," .NE. ",(col+1.5, col+2.5)
                  ERROR STOP 12
               else if (A2(row,col)%z .NE. (row+col+1.5, row+col+2.5)) then
                  print *,image,":",A2(row,col)%y," .NE. ",(row+col+1.5, row+col+2.5)
                  ERROR STOP 13
               else if (A2(row,col)%radius .NE. (row+col+2+1.5, row+col+2+1.5)) then
                  print *,image,":",A2(row,col)%y," .NE. ",(row+col+2+1.5, row+col+2+1.5)
                  ERROR STOP 14
               else if (A2(row,col)%color .NE. (col+2+1.5, col+2+1.5)) then
                  print *,image,":",A2(row,col)%y," .NE. ",(col+2+1.5, col+2+1.5)
                  ERROR STOP 15
               end if
            end do
            if (A(row) .NE. (row+1.5, row+2.5)) then
               print *,image,":",A(row)," .NE. ",(row+1.5, row+2.5)
               ERROR STOP 101
            end if
            if (A1(row)%x .NE. (row+1.5, row+2.5)) then
               print *,image,":",A1(row)%x," .NE. ",(row+1.5, row+2.5)
               ERROR STOP 21
            else if (A1(row)%y .NE. (col+1.5, col+2.5)) then
               print *,image,":",A1(row)%y," .NE. ",(col+1.5, col+2.5)
               ERROR STOP 22
            else if (A1(row)%z .NE. (row+col+1.5, row+col+2.5)) then
               print *,image,":",A1(row)%y," .NE. ",(row+col+1.5, row+col+2.5)
               ERROR STOP 23
            else if (A1(row)%radius .NE. (row+col+1+1.5, row+col+1+1.5)) then
               print *,image,":",A1(row)%y," .NE. ",(row+col+1+1.5, row+col+1+1.5)
               ERROR STOP 24
            else if (A1(row)%color .NE. (col+1+1.5, col+1+1.5)) then
               print *,image,":",A1(row)%y," .NE. ",(col+1+1.5, col+1+1.5)
               ERROR STOP 25
            end if
         end do
      else if (image .GT. images_processesed) then
         do row = 1, SIZE
            do col = 1, SIZE
               if (A2(row,col)%x .NE. (-300.0,-300.0) .AND. A2(row,col)%y .NE. (-300.0,-300.0) .AND. A2(row,col)%z .NE. (-300.0,-300.0) .AND. A2(row,col)%radius .NE. (-300.0,-300.0) .AND. A2(row,col)%color .NE. (-300.0,-300.0)) ERROR STOP 31
            end do
            if (A(row) .NE. (-300.0,-300.0)) ERROR STOP 102
            if (A1(row)%x .NE. (-300.0,-300.0) .AND. A1(row)%y .NE. (-300.0,-300.0) .AND. A1(row)%z .NE. (-300.0,-300.0) .AND. A1(row)%radius .NE. (-300.0,-300.0) .AND. A1(row)%color .NE. (-300.0,-300.0))ERROR STOP 32
         end do
      end if

   end subroutine verifyImagesProcessed

end module coa

program main

   use DT
   use coa

   implicit none

   integer, parameter :: SIZE = 3
   integer :: x1, y1, row, col, image, images, limit_X, limit_Y

   complex, save :: A(SIZE)[SIZE,*]
   type(SPHERE), save :: A1(SIZE)[SIZE,*]
   type(SPHERE), save :: A2(SIZE,SIZE)[SIZE,*]
   integer, save :: i[*]

   A = (-300.0, -300.0)
   i = 0;
   image = this_image()

   SYNC ALL

   if (image .EQ. 1) then
      limit_X = SIZE
      images = num_images()
      limit_Y = images/SIZE
      if (images .LT. SIZE) then
         limit_X = images
         limit_Y = 1
      end if
      do x1 = 1, limit_X
         do y1 = 1, limit_Y
            do row = 1, SIZE
               do col = 1, SIZE
                  A2(row,col)[x1,y1]%x = (row+1.5, row+2.5)
                  A2(row,col)[x1,y1]%y = (col+1.5, col+2.5)
                  A2(row,col)[x1,y1]%z = (row+col+1.5, row+col+2.5)
                  A2(row,col)[x1,y1]%radius = (row+col+2+1.5, row+col+2+1.5)
                  A2(row,col)[x1,y1]%color = (col+2+1.5, col+2+1.5)
               end do
               A(row)[x1,y1] = (row+1.5, row+2.5)
               A1(row)[x1,y1]%x = (row+1.5, row+2.5)
               A1(row)[x1,y1]%y = (col+1.5, col+2.5)
               A1(row)[x1,y1]%z = (row+col+1.5, row+col+2.5)
               A1(row)[x1,y1]%radius = (row+col+1+1.5, row+col+1+1.5)
               A1(row)[x1,y1]%color = (col+1+1.5, col+1+1.5)
            end do
            i = i + 1
         end do
      end do
      print *,image,":IMAGES PROCESSED:",i
   end if

   SYNC ALL
   ! make sure every image gets the number of images processed for verification
   i = i[1]

   print *,image,":ASSIGNED A :",A
   print *,image,":ASSIGNED A1:",A1
   print *,image,":ASSIGNED A2:",A2
   call verifyImagesProcessed(A, A1, A2, i, image, SIZE)
   print *,image,":VERIFIED A :",A
   print *,image,":VERIFIED A1:",A1
   print *,image,":VERIFIED A2:",A2

end program main
