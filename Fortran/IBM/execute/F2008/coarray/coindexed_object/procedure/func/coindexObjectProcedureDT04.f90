!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectProcedureDT04.f
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
!*  This program tests the coindex object procedure calls of derived type
!*
!*  Uses corank of 2.
!*  Setting data in image 1 for other images. The number of images should be
!*  either less than SIZE or multiple of SIZE. The extra number of images
!*  will be ignored.
!*  Dummy procedures
!*
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   ! Sphere type
   type SPHERE
      integer :: x = -300
      integer :: y = -300
      integer :: z = -300
      integer :: radius = -300
      integer :: color = -300
   end type SPHERE

end module DT

module coa

   use DT
   contains

   integer function verifyImagesProcessed(A1, A2, image, SIZE)
      type(SPHERE), intent(in) :: A1(:)[*]
      type(SPHERE), intent(in) :: A2(:,:)[*]
      integer, intent(in) :: image, SIZE
      integer :: row, col, res = 0

      if (image .LT. images_processesed) then
         do row = 1, SIZE
            do col = 1, SIZE
               if (A2(row,col)[image]%x .NE. row) then
                  print *,image,":",A2(row,col)[image]%x," .NE. ",row
                  res = 11
               else if (A2(row,col)%y .NE. col) then
                  print *,image,":",A2(row,col)[image]%y," .NE. ",col
                  res = 12
               else if (A2(row,col)[image]%z .NE. row+col) then
                  print *,image,":",A2(row,col)%y," .NE. ",row+col
                  res = 13
               else if (A2(row,col)[image]%radius .NE. row+col+2) then
                  print *,image,":",A2(row,col)[image]%y," .NE. ",row+col+2
                  res = 14
               else if (A2(row,col)[image]%color .NE. col+2) then
                  print *,image,":",A2(row,col)[image]%y," .NE. ",col+2
                  res = 15
               end if
            end do
            if (A1(row)[image]%x .NE. row) then
               print *,image,":",A1(row)[image]%x," .NE. ",row
               res = 21
            else if (A1(row)[image]%y .NE. col) then
               print *,image,":",A1(row)[image]%y," .NE. ",col
               res = 22
            else if (A1(row)[image]%z .NE. row+col) then
               print *,image,":",A1(row)[image]%y," .NE. ",row+col
               res = 23
            else if (A1(row)[image]%radius .NE. row+col+1) then
               print *,image,":",A1(row)[image]%y," .NE. ",row+col+1
               res = 24
            else if (A1(row)[image]%color .NE. col+1) then
               print *,image,":",A1(row)[image]%y," .NE. ",col+1
               res = 25
            end if
         end do
      end if

      verifyImagesProcessed = res
   end function verifyImagesProcessed

   integer function ret(r)
      integer, intent(in) :: r
      ret = r
   end function ret

end module coa

program main

   use DT
   use coa

   implicit none

   integer, parameter :: SIZE = 3
   integer :: row, col, image, limit_X, limit_Y

   type(SPHERE), save :: A1(SIZE)[SIZE,*]
   type(SPHERE), save :: A2(SIZE,SIZE)[SIZE,*]

   image = this_image()

            do row = 1, SIZE
               do col = 1, SIZE
                  A2(row,col)%x = row
                  A2(row,col)%y = col
                  A2(row,col)%z = row + col
                  A2(row,col)%radius = row + col + 2
                  A2(row,col)%color = col + 2
               end do
               A1(row)%x = ret(row)
               A1(row)%y = ret(col)
               A1(row)%z = ret(row + col)
               A1(row)%radius = row + col + 1
               A1(row)%color = col + 1
            end do

   print *,image,":ASSIGNED A1:",A1
   print *,image,":ASSIGNED A2:",A2
   call verifyResult(verifyImagesProcessed, A1, A2, image, SIZE)

   contains
   
   subroutine verifyResult(dummy_proc, A1, A2, image, SIZE)
      procedure(verifyImagesProcessed) :: dummy_proc
      type(SPHERE), intent(in) :: A1(:)[*]
      type(SPHERE), intent(in) :: A2(:,:)[*]
      integer, intent(in) :: image, SIZE
      integer :: res

      res = dummy_proc(A1, A2, image, SIZE)
      if (res .NE. 0) then
         print *,res
         ERROR STOP 103
      else
         print *,image,":VERIFIED A1:",A1
         print *,image,":VERIFIED A2:",A2
      end if
   end subroutine verifyResult

end program main
