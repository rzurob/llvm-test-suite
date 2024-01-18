!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectProcedureDT02.f
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
!*  Uses corank of 1.
!*  Use of coarrays as drived type in a module.
!*  Global assignment.
!*  Works with only even number of images.
!*  The extra or less number of images will be ignored.
!*  Data from images 1,3,... is assigned to images 2,4,...
!*  Data assigned is verified by images 1,3,...
!*  External procedures
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

   ! Dummy type
   type DUMMY
      integer :: i
   end type DUMMY

   contains

   subroutine assignData(A1, A2, image, next_image)
      type(SPHERE), dimension(:), intent(inout) :: A1[*]
      type(SPHERE), dimension(:,:,:), intent(inout) :: A2[*]
      integer, intent(in) :: image, next_image
      A1(:)[next_image] = A1(:)[image]
      A2(:,:,:)[next_image] = A2(:,:,:)[image]
      print *,"ASSIGNED: ",image,":A1:",A1(:)[image]
      print *,"ASSIGNED: ",next_image,":A1:",A1(:)[next_image]
      print *,"ASSIGNED: ",image,":A2:",A2(:,:,:)[image]
      print *,"ASSIGNED: ",next_image,":A2:",A2(:,:,:)[next_image]
   end subroutine assignData

   subroutine verifyAssignment(A1, A2, A3, A4, image, next_image, SIZE)
      type(SPHERE), dimension(:), intent(in) :: A1
      type(SPHERE), dimension(:), intent(in) :: A2
      type(SPHERE), dimension(:,:,:), intent(in) :: A3
      type(SPHERE), dimension(:,:,:), intent(in) :: A4
      integer, intent(in) :: image, next_image, SIZE
      integer x1, y1, z1
      do x1 = 1, SIZE
         if (A1(x1)%x .NE. A2(x1)%x) then
            print *,A1(x1)%x," .NE. ",A2(x1)%x
            ERROR STOP 101
         else if (A1(x1)%y .NE. A2(x1)%y) then
            print *,A1(x1)%y," .NE. ",A2(x1)%y
            ERROR STOP 102
         else if (A1(x1)%z .NE. A2(x1)%z) then
            print *,A1(x1)%z," .NE. ",A2(x1)%z
            ERROR STOP 103
         else if (A1(x1)%radius .NE. A2(x1)%radius) then
            print *,A1(x1)%radius," .NE. ",A2(x1)%radius
            ERROR STOP 104
         else if (A1(x1)%color .NE. A2(x1)%color) then
            print *,A1(x1)%color," .NE. ",A2(x1)%color
            ERROR STOP 105
         end if
         do y1 = 1, SIZE
            do z1 = 1, SIZE
               if (A3(x1,y1,z1)%x .NE. A4(x1,y1,z1)%x) then
                  print *,A3(x1,y1,z1)%x," .NE. ",A4(x1,y1,z1)%x
                  ERROR STOP 201
               else if (A3(x1,y1,z1)%y .NE. A4(x1,y1,z1)%y) then
                  print *,A3(x1,y1,z1)%y," .NE. ",A4(x1,y1,z1)%y
                  ERROR STOP 202
               else if (A3(x1,y1,z1)%z .NE. A4(x1,y1,z1)%z) then
                  print *,A3(x1,y1,z1)%z," .NE. ",A4(x1,y1,z1)%z
                  ERROR STOP 203
               else if (A3(x1,y1,z1)%radius .NE. A4(x1,y1,z1)%radius) then
                  print *,A3(x1,y1,z1)%radius," .NE. ",A4(x1,y1,z1)%radius
                  ERROR STOP 204
               else if (A3(x1,y1,z1)%color .NE. A4(x1,y1,z1)%color) then
                  print *,A3(x1,y1,z1)%color," .NE. ",A4(x1,y1,z1)%color
                  ERROR STOP 205
               end if
            end do
         end do
      end do
      print *,"VERIFIED: ",image,":A1:",A1
      print *,"VERIFIED: ",next_image,":A1:",A2
      print *,"VERIFIED: ",image,":A2:",A3
      print *,"VERIFIED: ",next_image,":A2:",A4
   end subroutine verifyAssignment

   subroutine verifyData(A, image, what)
      type(DUMMY), dimension(:,:), intent(in) :: A[*]
      integer, intent(in) :: image, what
      type(DUMMY), dimension(6) :: A1
      integer :: x1, y1

      A1 = reshape(A, shape(A1))

      print *,"TO BE VERIFIED: ",image,":A:",A1,":",SIZE(A1)
      do x1 = 1, SIZE(A1)
         if (A1(x1)%i .NE. what) then
            print *,A1(x1)%i," .NE. ",what
            ERROR STOP 201
         end if
      end do
      print *,"VERIFIED: ",image,":A:",A

   end subroutine verifyData

end module DT

program main

   use DT

   implicit none

   integer, parameter :: SIZE = 3
   integer row, col, x1, images, image, next_image

   type(SPHERE) :: p(SIZE)
   type(SPHERE), save :: p1(SIZE)[*]
   type(SPHERE), save :: p2(SIZE,SIZE,SIZE)[*]
   type(DUMMY), save :: p3(SIZE,SIZE)[*]
   image = this_image()

   ! Wait for all images to initialize the derived type data
   ! to synchronize the access of data to/from other images
   SYNC ALL

   ! image is odd (1,3,5,7,......)
   ! next_image is even (2,4,6,8,......)
   if (mod(image, 2) .EQ. 0) then
      next_image = image
      image = image - 1
      !
      ! Initialization of the data
      !
      do row = 1, SIZE
         p(row)%x = row + image
         p(row)%y = row + image + 1
         p(row)%z = row + image + 2
         p(row)%radius = row + image + 3
         p(row)%color = row + image + 4
         p1(row)[image]%x = row + image
         p1(row)[image]%y = row + image + 1
         p1(row)[image]%z = row + image + 2
         p1(row)[image]%radius = row + image + 3
         p1(row)[image]%color = row + image + 4
         do col = 1, SIZE
            do x1 = 1, SIZE
               p2(row,col,x1)[image]%x = row + col + x1 + image
               p2(row,col,x1)[image]%y = row + col + x1 + image + 1
               p2(row,col,x1)[image]%z = row + col + x1 + image + 2
               p2(row,col,x1)[image]%radius = row + col + x1 + image + 3
               p2(row,col,x1)[image]%color = row + col + x1 + image + 4
            end do
         end do
      end do

      call assignData(p1, p2, image, next_image)

      ! Verify the assignment
      ! Comparing the coarray "p1" with the local array "p"
      ! to make sure the coarray "p1" gets the same values as array "p"
      call verifyAssignment(p(:), p1(:)[next_image], p2(:,:,:)[image], p2(:,:,:)[next_image], image, next_image, SIZE);
      p3%i = 101
      call verifyData(p3(1:3,2:3), image, 101);
      p3%i = 103
      call verifyData(p3(2:3,1:3), image, 103);
   end if

   SYNC ALL

end program main

