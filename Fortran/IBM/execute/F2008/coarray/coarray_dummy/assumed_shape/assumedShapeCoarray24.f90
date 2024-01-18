!*  ============================================================================
!*
!*  TEST CASE NAME             : assumedShapeCoarray24.f
!*
!*  DATE                       : 2011-02-18
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments assumed shape
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 386330
!*
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
!*  Use of coarrays as drived type. Uses same corank but different shapes.
!*  Copying/moving data among images. Works with only even number of images.
!*  The extra or less number of images will be ignored.
!*  Data from images 1,3,... is copied to images 2,4,...
!*  Data copying is verified by images 1,3,...
!*  Type used real
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   ! Base type
   type POINT
      integer :: x
      integer :: y
      integer :: z
      integer :: dir
   end type POINT

end module DT

module asc

   use DT
   contains

   subroutine copyData(A, A1, A2, image, next_image)
      type(PLANE), dimension(:,:), intent(in) :: A
      type(PLANE), dimension(:,:), intent(inout) :: A1[*]
      type(PLANE), dimension(:,:,:,:), intent(inout) :: A2[*]
      integer, intent(in) :: image, next_image
      A1(:,:)[next_image] = A1(:,:)[image]
      A2(:,:,:,:)[next_image] = A2(:,:,:,:)[image]
      print *,"PRINTING: ",next_image,":A = A1:",A(:,:)
      print *,"PRINTING: ",image,":A1 = A:",A1(:,:)[image]
      print *,"PRINTING: ",next_image,":A1 = A:",A1(:,:)[next_image]
      print *,"PRINTING: ",image,":A2:",A2(:,:,:,:)[image]
      print *,"PRINTING: ",next_image,":A2:",A2(:,:,:,:)[next_image]
   end subroutine copyData

   subroutine verifyCopy(A1, A2, A3, A4, image, next_image, SIZE)
      type(PLANE), dimension(:,:), intent(in) :: A1
      type(PLANE), dimension(:,:), intent(in) :: A2
      type(PLANE), dimension(:,:,:,:), intent(in) :: A3
      type(PLANE), dimension(:,:,:,:), intent(in) :: A4
      integer, intent(in) :: image, next_image, SIZE
      integer x1, y1, z1, w1
      do x1 = 1, SIZE
         do y1 = 1, SIZE
            if (A1(x1,y1)%x .NE. A2(x1,y1)%x) then
               print *,A1(x1,y1)%x," .NE. ",A2(x1,y1)%x
               ERROR STOP 101
            end if
            if (A1(x1,y1)%y .NE. A2(x1,y1)%y) then
               print *,A1(x1,y1)%y," .NE. ",A2(x1,y1)%y
               ERROR STOP 102
            end if
            if (A1(x1,y1)%z .NE. A2(x1,y1)%z) then
               print *,A1(x1,y1)%z," .NE. ",A2(x1,y1)%z
               ERROR STOP 103
            end if
            if (A1(x1,y1)%dir .NE. A2(x1,y1)%dir) then
               print *,A1(x1,y1)%dir," .NE. ",A2(x1,y1)%dir
               ERROR STOP 104
            end if
            do z1 = 1, SIZE
               do w1 = 1, SIZE
                  if (A3(x1,y1,z1,w1)%x .NE. A4(x1,y1,z1,w1)%x) then
                     print *,A3(x1,y1,z1,w1)%x," .NE. ",A4(x1,y1,z1,w1)%x
                     ERROR STOP 201
                  end if
                  if (A3(x1,y1,z1,w1)%y .NE. A4(x1,y1,z1,w1)%y) then
                     print *,A3(x1,y1,z1,w1)%y," .NE. ",A4(x1,y1,z1,w1)%y
                     ERROR STOP 202
                  end if
                  if (A3(x1,y1,z1,w1)%z .NE. A4(x1,y1,z1,w1)%z) then
                     print *,A3(x1,y1,z1,w1)%z," .NE. ",A4(x1,y1,z1,w1)%z
                     ERROR STOP 203
                  end if
                  if (A3(x1,y1,z1,w1)%dir .NE. A4(x1,y1,z1,w1)%dir) then
                     print *,A3(x1,y1,z1,w1)%dir," .NE. ",A4(x1,y1,z1,w1)%dir
                     ERROR STOP 204
                  end if
               end do
            end do
         end do
      end do
      print *,"VERIFIED: ",image,":A1:",A1
      print *,"VERIFIED: ",next_image,":A1:",A2
      print *,"VERIFIED: ",image,":A2:",A3
      print *,"VERIFIED: ",next_image,":A2:",A4
   end subroutine verifyCopy

end module asc

program main

   use DT
   use asc

   implicit none

   integer, parameter :: SIZE = 3
   integer row, col, x1, y1, image, next_image
   type(PLANE) :: p(SIZE,SIZE)
   type(PLANE), save :: p1(SIZE,SIZE)[*], p2(SIZE,SIZE,SIZE,SIZE)[*]

   ! image is odd (1,3,5,7,......)
   ! next_image is even (2,4,6,8,......)
   image = this_image()

   ! Wait for all images to initialize the derived type data
   ! to synchronize the access of data to/from other images
   SYNC ALL

   if (mod(image, 2) .EQ. 0) then
      next_image = image
      image = image - 1
      !
      ! Initialization of the data
      !
      do row = 1, SIZE
         do col = 1, SIZE
            p(row,col)%x = row
            p(row,col)%y = col
            p(row,col)%z = image
            p(row,col)%dir = image
            p1(row,col)[image]%x = row
            p1(row,col)[image]%y = col
            p1(row,col)[image]%z = image
            p1(row,col)[image]%dir = image
            do x1 = 1, SIZE
               do y1 = 1, SIZE
                  p2(row,col,x1,y1)[image]%x = row + x1 + y1
                  p2(row,col,x1,y1)[image]%y = col + x1 + y1
                  p2(row,col,x1,y1)[image]%z = image * x1 + y1
                  p2(row,col,x1,y1)[image]%dir = image * x1 + y1
               end do
            end do
         end do
      end do

      call copyData(p, p1, p2, image, next_image)

      ! Verify the copy
      ! Comparing the coarray "p1" with the local array "p"
      ! to make sure the coarray "p1" gets the same values as array "p"
      ! If not that means the derived type access/setting values is not working in coarrays
      call verifyCopy(p(:,:), p1(:,:)[next_image], p2(:,:,:,:)[image], p2(:,:,:,:)[next_image], image, next_image, SIZE);
   end if

   SYNC ALL

end program main
