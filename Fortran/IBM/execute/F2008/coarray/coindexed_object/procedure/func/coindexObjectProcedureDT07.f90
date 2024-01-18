!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectProcedureDT07.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2011-05-20
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coindex object Procedure Call
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type
!*
!*  REFERENCE                  : Feature Number 388003
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
!*  Procedure pointer.
!*
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   ! Sphere type
   type SPHERE
      real :: x = -300.0
      real :: y = -300.0
      real :: z = -300.0
      real :: radius = -300.0
      real :: color = -300.0
   end type SPHERE

end module DT

module coa

   use DT
   contains

   subroutine verifyImagesProcessed(A1, A2, image, SIZE)
      type(SPHERE), intent(in) :: A1(:)[*]
      type(SPHERE), intent(in) :: A2(:,:)[*]
      integer, intent(in) :: image, SIZE
      integer :: row, col

         do row = 1, SIZE
            do col = 1, SIZE
               if (A2(row,col)[image]%x .NE. row+1.5) then
                  print *,image,":",A2(row,col)[image]%x," .NE. ",row+1.5
                  ERROR STOP 11
               else if (A2(row,col)%y .NE. col+1.5) then
                  print *,image,":",A2(row,col)[image]%y," .NE. ",col+1.5
                  ERROR STOP 12
               else if (A2(row,col)[image]%z .NE. row+col+1.5) then
                  print *,image,":",A2(row,col)[image]%y," .NE. ",row+col+1.5
                  ERROR STOP 13
               else if (A2(row,col)[image]%radius .NE. row+col+2+1.5) then
                  print *,image,":",A2(row,col)[image]%y," .NE. ",row+col+2+1.5
                  ERROR STOP 14
               else if (A2(row,col)[image]%color .NE. col+2+1.5) then
                  print *,image,":",A2(row,col)[image]%y," .NE. ",col+2+1.5
                  ERROR STOP 15
               end if
            end do
            if (A1(row)[image]%x .NE. row+1.5) then
               print *,image,":",A1(row)[image]%x," .NE. ",row+1.5
               ERROR STOP 21
            else if (A1(row)[image]%y .NE. col+1.5) then
               print *,image,":",A1(row)[image]%y," .NE. ",col+1.5
               ERROR STOP 22
            else if (A1(row)[image]%z .NE. row+col+1.5) then
               print *,image,":",A1(row)[image]%y," .NE. ",row+col+1.5
               ERROR STOP 23
            else if (A1(row)[image]%radius .NE. row+col+1+1.5) then
               print *,image,":",A1(row)[image]%y," .NE. ",row+col+1+1.5
               ERROR STOP 24
            else if (A1(row)[image]%color .NE. col+1+1.5) then
               print *,image,":",A1(row)[image]%y," .NE. ",col+1+1.5
               ERROR STOP 25
            end if
         end do

   end subroutine verifyImagesProcessed

   subroutine verifyDataByImageOne(A1, A2, SIZE)
      type(SPHERE), intent(in) :: A1(:)
      type(SPHERE), intent(in) :: A2(:,:)
      integer, intent(in) :: SIZE
      integer :: row, col

         do row = 1, SIZE
            do col = 1, SIZE
               if (A2(row,col)%x .NE. row+1.5) then
                  print *,image,":",A2(row,col)%x," .NE. ",row+1.5
                  ERROR STOP 11
               else if (A2(row,col)%y .NE. col+1.5) then
                  print *,image,":",A2(row,col)%y," .NE. ",col+1.5
                  ERROR STOP 12
               else if (A2(row,col)%z .NE. row+col+1.5) then
                  print *,image,":",A2(row,col)%y," .NE. ",row+col+1.5
                  ERROR STOP 13
               else if (A2(row,col)%radius .NE. row+col+2+1.5) then
                  print *,image,":",A2(row,col)%y," .NE. ",row+col+2+1.5
                  ERROR STOP 14
               else if (A2(row,col)%color .NE. col+2+1.5) then
                  print *,image,":",A2(row,col)%y," .NE. ",col+2+1.5
                  ERROR STOP 15
               end if
            end do
            if (A1(row)%x .NE. row+1.5) then
               print *,image,":",A1(row)%x," .NE. ",row+1.5
               ERROR STOP 21
            else if (A1(row)%y .NE. col+1.5) then
               print *,image,":",A1(row)%y," .NE. ",col+1.5
               ERROR STOP 22
            else if (A1(row)%z .NE. row+col+1.5) then
               print *,image,":",A1(row)%y," .NE. ",row+col+1.5
               ERROR STOP 23
            else if (A1(row)%radius .NE. row+col+1+1.5) then
               print *,image,":",A1(row)%y," .NE. ",row+col+1+1.5
               ERROR STOP 24
            else if (A1(row)%color .NE. col+1+1.5) then
               print *,image,":",A1(row)%y," .NE. ",col+1+1.5
               ERROR STOP 25
            end if
         end do

   end subroutine verifyDataByImageOne

end module coa

program main

   use DT
   use coa

   implicit none

   integer, parameter :: SIZE = 3
   integer :: x1, y1, row, col, image, images, limit_X, limit_Y

   type(SPHERE), save :: A1(SIZE)[SIZE,*]
   type(SPHERE), save :: A2(SIZE,SIZE)[SIZE,*]

   procedure(verifyImagesProcessed), pointer :: p_ptr => NULL()

   p_ptr => verifyImagesProcessed

   image = this_image()

            do row = 1, SIZE
               do col = 1, SIZE
                  A2(row,col)%x = row + 1.5
                  A2(row,col)%y = col + 1.5
                  A2(row,col)%z = row + col + 1.5
                  A2(row,col)%radius = row + col + 2 + 1.5
                  A2(row,col)%color = col + 2 + 1.5
               end do
               A1(row)%x = row + 1.5
               A1(row)%y = col + 1.5
               A1(row)%z = row + col + 1.5
               A1(row)%radius = row + col + 1 + 1.5
               A1(row)%color = col + 1 + 1.5
            end do

   print *,image,":ASSIGNED A1:",A1
   print *,image,":ASSIGNED A2:",A2
   call p_ptr(A1, A2, image, SIZE)

   SYNC ALL

   ! Image 1 verifying data in all other images
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
            call verifyDataByImageOne(A1[x1,y1], A2[x1,y1], SIZE)
         end do
      end do
   end if
   print *,image,":VERIFIED A1:",A1
   print *,image,":VERIFIED A2:",A2

end program main
