!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectProcedureDT03.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2011-05-20
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coindex object Procedure Calls
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
!*  This program tests the coindex object procedure calls of derived type
!*
!*  Uses corank of 2.
!*  Internal Procedures.
!*
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   ! Sphere type
   type SPHERE
      integer :: x
      integer :: y
      integer :: z
      integer :: radius
      integer :: color
   end type SPHERE

end module DT

program main

   use DT

   implicit none

   integer, parameter :: SIZE = 3
   integer :: x1, y1, row, col, image, images, limit_X, limit_Y

   integer, save :: A(SIZE)[SIZE,*]
   type(SPHERE), save :: A1(SIZE)[SIZE,*]
   type(SPHERE), save :: A2(SIZE,SIZE)[SIZE,*]

   image = this_image()
   A = image

   ! Find the current image in the rectangular pattern
   ! and then access the current image using the image selector
   limit_X = SIZE
   images = num_images()
   limit_Y = images/SIZE
   if (images .LT. SIZE) then
      limit_X = images
      limit_Y = 1
   end if
   ! Do while using if
   do x1 = 1, limit_X
      do y1 = 1, limit_Y
         if (image .EQ. A(1)[x1,y1]) then
            print *,"DEBUG:",image,":",x1,",",y1
            do row = 1, SIZE
               do col = 1, SIZE
                  A2(row,col)[x1,y1]%x = row
                  A2(row,col)[x1,y1]%y = col
                  A2(row,col)[x1,y1]%z = row + col
                  A2(row,col)[x1,y1]%radius = row + col + 2
                  A2(row,col)[x1,y1]%color = col + 2
               end do
               A(row)[x1,y1] = row
               A1(row)[x1,y1]%x = row
               A1(row)[x1,y1]%y = col
               A1(row)[x1,y1]%z = row + col
               A1(row)[x1,y1]%radius = row + col + 1
               A1(row)[x1,y1]%color = col + 1
            end do
            print *,image,":ASSIGNED A :",A
            print *,image,":ASSIGNED A1:",A1
            print *,image,":ASSIGNED A2:",A2
            call verifyImagesProcessed(A[x1,y1], A1[x1,y1], A2[x1,y1], image, SIZE)
            print *,image,":VERIFIED A :",A
            print *,image,":VERIFIED A1:",A1
            print *,image,":VERIFIED A2:",A2
            ! we are done break the loop
            go to 1001
         end if
      end do
   end do

   1001 continue

   contains

   subroutine verifyImagesProcessed(A, A1, A2, image, SIZE)
      integer, intent(in) :: A(:)
      type(SPHERE), intent(in) :: A1(:)
      type(SPHERE), intent(in) :: A2(:,:)
      integer, intent(in) :: image, SIZE
      integer :: row, col

         do row = 1, SIZE
            do col = 1, SIZE
               if (A2(row,col)%x .NE. row) then
                  print *,image,":",A2(row,col)%x," .NE. ",row
                  ERROR STOP 11
               else if (A2(row,col)%y .NE. col) then
                  print *,image,":",A2(row,col)%y," .NE. ",col
                  ERROR STOP 12
               else if (A2(row,col)%z .NE. row+col) then
                  print *,image,":",A2(row,col)%y," .NE. ",row+col
                  ERROR STOP 13
               else if (A2(row,col)%radius .NE. row+col+2) then
                  print *,image,":",A2(row,col)%y," .NE. ",row+col+2
                  ERROR STOP 14
               else if (A2(row,col)%color .NE. col+2) then
                  print *,image,":",A2(row,col)%y," .NE. ",col+2
                  ERROR STOP 15
               end if
            end do
            if (A(row) .NE. row) then
               print *,image,":",A(row)," .NE. ",row
               ERROR STOP 101
            end if
            if (A1(row)%x .NE. row) then
               print *,image,":",A1(row)%x," .NE. ",row
               ERROR STOP 21
            else if (A1(row)%y .NE. col) then
               print *,image,":",A1(row)%y," .NE. ",col
               ERROR STOP 22
            else if (A1(row)%z .NE. row+col) then
               print *,image,":",A1(row)%y," .NE. ",row+col
               ERROR STOP 23
            else if (A1(row)%radius .NE. row+col+1) then
               print *,image,":",A1(row)%y," .NE. ",row+col+1
               ERROR STOP 24
            else if (A1(row)%color .NE. col+1) then
               print *,image,":",A1(row)%y," .NE. ",col+1
               ERROR STOP 25
            end if
         end do

   end subroutine verifyImagesProcessed

end program main
