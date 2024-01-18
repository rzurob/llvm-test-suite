!*  ============================================================================
!*
!*  TEST CASE NAME             : coindexObjectAssignmentDT10.f
!*
!*  DATE                       : 2011-03-11
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coindex object assignment statement
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type
!*
!*  REFERENCE                  : Feature Number 386924
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Uses corank of 1.
!*  Testing derived type co-arrays with parameters.
!*  Assignment using derived type.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module DT

   type circle
      integer :: x
      integer :: y
      integer :: radius
      integer :: color
   end type circle

   type rectangle
      integer :: length
      integer :: width
      integer :: color
   end type rectangle

   type cube
      integer :: edgeLength
      integer :: color
   end type cube

   type shapes(d1, d2, d3)
      integer, kind :: d1, d2, d3
      type(circle) :: circle(d1)
      type(rectangle) :: rectangle(d2)
      type(cube) :: cube(d3)
   end type shapes

end module DT

program main

   use DT

   implicit none

   integer, parameter :: SIZE = 3
   integer :: x1, y1, row, col, image, images, limit_X, limit_Y

   integer, save :: S(SIZE)[SIZE,*]
   type(shapes(1,2,3)), save :: S1(SIZE)[SIZE,*]
   type(shapes(4,5,6)), save :: S2(SIZE,SIZE)[SIZE,*]

   image = this_image()
   S = image

   ! Find the current image in the rectangle pattern
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
         if (image .EQ. S(1)[x1,y1]) then
            print *,"DEBUG:",image,":",x1,",",y1
            do row = 1, SIZE
               do col = 1, SIZE
                  call assignS2WithoutImageSelector()
               end do
               S(row) = row+image
               call assignS1WithoutImageSelector()
            end do
            print *,image,":WITHOUT IMAGE SELECTOR S :",S
            print *,image,":WITHOUT IMAGE SELECTOR S1:",S1
            print *,image,":WITHOUT IMAGE SELECTOR S2:",S2
            ! we are done break the loop
            go to 1111
         end if
      end do
   end do

   1111 continue

   S = image
   S1 = shapes(1,2,3) ( circle(0,0,0,0), rectangle(0,0,0), cube(0,0) )
   S2 = shapes(4,5,6) ( circle(0,0,0,0), rectangle(0,0,0), cube(0,0) )
   print *,image,":IMAGED S :",S
   print *,image,":ZEROED S1:",S1
   print *,image,":ZEROED S2:",S2

   ! Find the current image in the rectangle pattern
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
         if (image .EQ. S(1)[x1,y1]) then
            print *,"DEBUG:",image,":",x1,",",y1
            do row = 1, SIZE
               do col = 1, SIZE
                  call assignS2WithImageSelector()
               end do
               S(row)[x1,y1] = row+image
               call assignS1WithImageSelector()
            end do
            print *,image,":WITH IMAGE SELECTOR S :",S
            print *,image,":WITH IMAGE SELECTOR S1:",S1
            print *,image,":WITH IMAGE SELECTOR S2:",S2
            ! we are done break the loop
            go to 1113
         end if
      end do
   end do

   1113 continue

   contains

   subroutine assignS2WithoutImageSelector()
      S2(row,col)%circle(1) = circle(row, col, row+col+1, col+1+image)
      S2(row,col)%circle(2) = circle(row, col, row+col+2, col+2+image)
      S2(row,col)%circle(3) = circle(row, col, row+col+3, col+3+image)
      S2(row,col)%circle(4) = circle(row, col, row+col+4, col+4+image)
      S2(row,col)%rectangle(1) = rectangle(row+5, col+7, col+1+image)
      S2(row,col)%rectangle(2) = rectangle(row+5, col+7, col+2+image)
      S2(row,col)%rectangle(3) = rectangle(row+5, col+7, col+3+image)
      S2(row,col)%rectangle(4) = rectangle(row+5, col+7, col+4+image)
      S2(row,col)%rectangle(5) = rectangle(row+5, col+7, col+5+image)
      S2(row,col)%cube(1) = cube(row+9, col+1+image)
      S2(row,col)%cube(2) = cube(row+9, col+2+image)
      S2(row,col)%cube(3) = cube(row+9, col+3+image)
      S2(row,col)%cube(4) = cube(row+9, col+4+image)
      S2(row,col)%cube(5) = cube(row+9, col+5+image)
      S2(row,col)%cube(6) = cube(row+9, col+6+image)
   end subroutine assignS2WithoutImageSelector

   subroutine assignS1WithoutImageSelector()
      S1(row)%circle(1) = circle(row, col, row+col+10, col+10+image)
      S1(row)%rectangle(1) = rectangle(row+50, col+70, col+10+image)
      S1(row)%rectangle(2) = rectangle(row+50, col+70, col+20+image)
      S1(row)%cube(1) = cube(row+90, col+10+image)
      S1(row)%cube(2) = cube(row+90, col+20+image)
      S1(row)%cube(3) = cube(row+90, col+30+image)
   end subroutine assignS1WithoutImageSelector

   subroutine assignS2WithImageSelector()
      S2(row,col)[x1,y1]%circle(1) = circle(row, col, row+col+1, col+1+image)
      S2(row,col)[x1,y1]%circle(2) = circle(row, col, row+col+2, col+2+image)
      S2(row,col)[x1,y1]%circle(3) = circle(row, col, row+col+3, col+3+image)
      S2(row,col)[x1,y1]%circle(4) = circle(row, col, row+col+4, col+4+image)
      S2(row,col)[x1,y1]%rectangle(1) = rectangle(row+5, col+7, col+1+image)
      S2(row,col)[x1,y1]%rectangle(2) = rectangle(row+5, col+7, col+2+image)
      S2(row,col)[x1,y1]%rectangle(3) = rectangle(row+5, col+7, col+3+image)
      S2(row,col)[x1,y1]%rectangle(4) = rectangle(row+5, col+7, col+4+image)
      S2(row,col)[x1,y1]%rectangle(5) = rectangle(row+5, col+7, col+5+image)
      S2(row,col)[x1,y1]%cube(1) = cube(row+9, col+1+image)
      S2(row,col)[x1,y1]%cube(2) = cube(row+9, col+2+image)
      S2(row,col)[x1,y1]%cube(3) = cube(row+9, col+3+image)
      S2(row,col)[x1,y1]%cube(4) = cube(row+9, col+4+image)
      S2(row,col)[x1,y1]%cube(5) = cube(row+9, col+5+image)
      S2(row,col)[x1,y1]%cube(6) = cube(row+9, col+6+image)
   end subroutine assignS2WithImageSelector

   subroutine assignS1WithImageSelector()
      S1(row)[x1,y1]%circle(1) = circle(row, col, row+col+10, col+10+image)
      S1(row)[x1,y1]%rectangle(1) = rectangle(row+50, col+70, col+10+image)
      S1(row)[x1,y1]%rectangle(2) = rectangle(row+50, col+70, col+20+image)
      S1(row)[x1,y1]%cube(1) = cube(row+90, col+10+image)
      S1(row)[x1,y1]%cube(2) = cube(row+90, col+20+image)
      S1(row)[x1,y1]%cube(3) = cube(row+90, col+30+image)
   end subroutine assignS1WithImageSelector

end program main
