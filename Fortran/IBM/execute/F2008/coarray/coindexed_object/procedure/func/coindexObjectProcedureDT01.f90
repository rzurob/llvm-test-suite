!*  ============================================================================
!*
!*  DATE                       : 2011-05-20
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coindex object Procedure Calls
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type
!*
!*  REFERENCE                  : Feature Number 388003
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TEST CONDITIONS  :
!*
!*  DESCRIPTION
!*
!*  Uses corank of 1.
!*  This program tests the coindex object procedure calls of derived type
!*  module procedure
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

module coa

   use DT
   contains

   subroutine verifyAssignment(A1, A2, A3, image, SIZE)
      type(SPHERE), intent(in) :: A1(:)
      type(SPHERE), intent(in) :: A2(:)
      type(SPHERE), intent(in) :: A3
      integer, intent(in) :: image, SIZE
      integer :: row

      if (image .LE. num_images()) then
         do row = 1, SIZE
            if (A1(row)%x .NE. A2(row)%x .OR. A1(row)%x .NE. row+image) then
               print *,image,":",A1(row)%x," .OR. ",A2(row)%x," .NE. ",row+image
               ERROR STOP 11
            else if (A1(row)%y .NE. A2(row)%y .OR. A1(row)%y .NE. row+image+1) then
               print *,image,":",A1(row)%y," .OR. ",A2(row)%y," .NE. ",row+image
               ERROR STOP 12
            else if (A1(row)%z .NE. A2(row)%z .OR. A1(row)%z .NE. row+image+2) then
               print *,image,":",A1(row)%z," .OR. ",A2(row)%z," .NE. ",row+image+2
               ERROR STOP 13
            else if (A1(row)%radius .NE. A2(row)%radius .OR. A1(row)%radius .NE. row+image+3) then
               print *,image,":",A1(row)%radius," .OR. ",A2(row)%radius," .NE. ",row+image+3
               ERROR STOP 14
            else if (A1(row)%color .EQ. A2(row)%color .AND. A1(row)%color .NE. row+image+4) then
               print *,image,":",A1(row)%color," .OR. ",A2(row)%color," .NE. ",row+image+4
               ERROR STOP 15
            end if
         end do
         if (A3%x .NE. row+image) then
            print *,image,":",A3%x," .NE. ",row+image
            ERROR STOP 16
         else if (A3%y .NE. row+image+1) then
            print *,image,":",A3%y," .NE. ",row+image+1
            ERROR STOP 17
         else if (A3%z .NE. row+image+2) then
            print *,image,":",A3%y," .NE. ",row+image+2
            ERROR STOP 18
         else if (A3%radius .NE. row+image+3) then
            print *,image,":",A3%radius," .NE. ",row+image+3
            ERROR STOP 19
         else if (A3%color .NE. row+image+4) then
            print *,image,":",A3%color," .NE. ",row+image+4
            ERROR STOP 20
         end if
      else
         print *,"Image number '",image,"' passed is greater than the number of images"
         ERROR STOP 101
      end if

   end subroutine verifyAssignment

end module coa

program main

   use DT
   use coa

   implicit none

   integer, parameter :: SIZE = 3
   integer :: row, image

   type(SPHERE), save :: A1(SIZE)[*]
   type(SPHERE), save :: A2(SIZE)[*]
   type(SPHERE), save :: A3[*]

   ! Assign data locally
   image = this_image()
   do row = 1, SIZE
      A1(row)[image]%x = row + image
      A1(row)[image]%y = row + image + 1
      A1(row)[image]%z = row + image + 2
      A1(row)[image]%radius = row + image + 3
      A1(row)[image]%color = row + image + 4
      A2(row)[image]%x = row + image
      A2(row)[image]%y = row + image + 1
      A2(row)[image]%z = row + image + 2
      A2(row)[image]%radius = row + image + 3
      A2(row)[image]%color = row + image + 4
   end do
   A3[image]%x = row + image
   A3[image]%y = row + image + 1
   A3[image]%z = row + image + 2
   A3[image]%radius = row + image + 3
   A3[image]%color = row + image + 4

   print *,image,":ASSIGNED A1:",A1[image]
   print *,image,":ASSIGNED A2:",A2[image]
   print *,image,":ASSIGNED A3:",A3[image]
   call verifyAssignment(A1[image], A2[image], A3[image], image, SIZE)
   print *,image,":VERIFIED A1:",A1[image]
   print *,image,":VERIFIED A2:",A2[image]
   print *,image,":VERIFIED A3:",A3[image]

end program main
