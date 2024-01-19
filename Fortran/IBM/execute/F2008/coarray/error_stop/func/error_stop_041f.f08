!*
!*  ===================================================================
!*
!*  TYPE                       : Functional test
!*  FEATURE                    : #351605.31 CAF - ERROR STOP statement
!*
!*  DATE                       : 28 August 2010
!*
!*  REQUIRED COMPILER OPTIONS  :
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : ERROR STOP interrupts circular shift
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program error_stop_prog
  implicit none
  integer       :: selfImage, endImage, numImages
  integer, save :: image_neighbors(2)[*]
  integer       :: leftImage, rightImage

  selfImage = this_image()
  numImages = num_images()
  endImage  = numImages/2
  if (endImage < 1) endImage = 1
  image_neighbors = 0

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

  if (selfImage == endImage) error stop "Correct end"
  sync all

  if ( image_neighbors(1) /= rightImage) then
     print *, "Error in image #",selfImage," rightImage is ",image_neighbors(1)," not ",rightImage
     stop
  end if

  if ( image_neighbors(2) /= leftImage) then
     print *, "Error in image #",selfImage," leftImage is ",image_neighbors(2)," not ",leftImage
     stop
  end if

  print *, "  All good in image #", selfImage, "-",image_neighbors," in not Error Stop"
  sync all   !<-To be removed when END PROGRAM implemented
end program error_stop_prog
