!*
!*  ===================================================================
!*
!*  TYPE                       : Functional test
!*  FEATURE                    : #351605.31 CAF - STOP statement
!*
!*  DATE                       : 19 Oct 2010
!*
!*  REQUIRED COMPILER OPTIONS  :
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : STOP initiating normal termination of circular shift
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program stop_prog
  implicit none
  integer       :: selfImage, endImage, numImages
  integer, save :: image_neighbors(2)[*]
  integer       :: leftImage, rightImage

  selfImage = this_image()
  numImages = num_images()
  if (numImages == 1) then
     print *, " STOP Correct end, NOT"
     print *, "  All good in image #", selfImage, "-",image_neighbors," in not Stop"
     stop "Only one . . ."
  end if
  endImage  = numImages/2
  if (endImage < 1) endImage = 1
  image_neighbors = 0
  sync all

  if (selfImage == numImages) then
     rightImage = 1
  else
     rightImage = selfImage + 1
  end if

  if (selfImage == 1) then
     leftImage = numImages
  else
     leftImage = selfImage - 1
  end if

  call sleep_(mod(selfImage,8)*3)

  image_neighbors(1)[leftImage]  = selfImage
  image_neighbors(2)[rightImage] = selfImage

  sync all
  if (selfImage == endImage) stop "Correct end, NOT"
!  if (selfImage == endImage) print *, " STOP Correct end, NOT"

  if ( image_neighbors(1) /= rightImage) then
     print *, "Error in image #",selfImage," rightImage is ",image_neighbors(1)," not ",rightImage
     error stop 77
  end if

  if ( image_neighbors(2) /= leftImage) then
     print *, "Error in image #",selfImage," leftImage is ",image_neighbors(2)," not ",leftImage
     error stop 88
  end if

  print *, "  All good in image #", selfImage, "-",image_neighbors," in not Stop"

end program stop_prog
