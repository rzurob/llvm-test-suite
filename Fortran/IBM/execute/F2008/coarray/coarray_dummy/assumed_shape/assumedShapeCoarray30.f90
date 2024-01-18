!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : assumedShapeCoarray30.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Shahid Alam
!*  DATE                       : 2011-02-24
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : CAF coarray dummy arguments assumed shape
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 386330
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
!*  This program tests the assumed shape dummy arguments coarrays
!*
!*  Uses different coranks and different shapes.
!*  Setting data in image 1 for other images. The number of images should be
!*  either less than SIZE or multiple of SIZE. The extra number of images
!*  will be ignored. Type used logical
!*
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module asc

   contains

   subroutine verifyImagesProcessed(A1, A2, images_processesed, image, SIZE)
      logical, intent(in) :: A1(SIZE)[SIZE,*]
      logical, intent(in) :: A2(SIZE,SIZE)[SIZE,*]
      integer, intent(in) :: images_processesed, image, SIZE
      integer :: row, col

      if (image .LT. images_processesed) then
         do row = 1, SIZE
            do col = 1, SIZE
               if (A2(row,col) .NEQV. .TRUE.) then
                  print *,image,":",A2(row,col)," .NEQV. ",.TRUE.
                  ERROR STOP 101
               end if
            end do
            if (A1(row) .NEQV. .TRUE.) then
               print *,image,":",A1(row)," .NEQV. ",.TRUE.
               ERROR STOP 102
            end if
         end do
      else if (image .GT. images_processesed) then
         do row = 1, SIZE
            do col = 1, SIZE
               if (A2(row,col) .NEQV. .FALSE.) then
                  print *,image,":",A2(row,col)," .NEQV. ",.FALSE.
                  ERROR STOP 103
               end if
            end do
            if (A1(row) .NEQV. .FALSE.) then
               print *,image,":",A1(row)," .NEQV. ",.FALSE.
               ERROR STOP 104
            end if
         end do
      end if

   end subroutine verifyImagesProcessed

end module asc

program main

   use asc

   implicit none

   integer, parameter :: SIZE = 3
   integer :: x, y, row, col, image, images, limit_X, limit_Y

   logical, save :: A1(SIZE)[SIZE,*]
   logical, save :: A2(SIZE,SIZE)[SIZE,*]
   integer, save :: i[*]

   i = 0;
   A1 = .FALSE.
   A2 = .FALSE.
   image = this_image()
   print *,image,":A1 BEFORE:",A1
   print *,image,":A2 BEFORE:",A2

   SYNC ALL

   if (image .EQ. 1) then
      limit_X = SIZE
      images = num_images()
      limit_Y = images/SIZE
      if (images .LT. SIZE) then
         limit_X = images
         limit_Y = 1
      end if
      do x = 1, limit_X
         do y = 1, limit_Y
            do row = 1, SIZE
               do col = 1, SIZE
                  A2(row,col)[x,y] = .TRUE.
               end do
               A1(row)[x,y] = .TRUE.
            end do
            i = i + 1
         end do
      end do
      print *,image,":IMAGES PROCESSED:",i
   end if

   SYNC ALL
   ! make sure every image gets the number of images processed for verification
   i = i[1]

   call verifyImagesProcessed(A1, A2, i, image, SIZE)

   print *,image,":A1 AFTER :",A1
   print *,image,":A2 AFTER :",A2

   SYNC ALL

end program main
