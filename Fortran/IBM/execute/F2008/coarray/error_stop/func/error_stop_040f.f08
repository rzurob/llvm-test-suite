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
!*  DESCRIPTION                : ERROR STOP interrupts ordered image work
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program error_stop_prog
  implicit none
  integer       :: selfImage, endImage, numImages
  integer, save :: co_counter[*]
  integer       :: i

  selfImage = this_image()
  numImages = num_images()
  endImage  = numImages/2
  if (endImage < 1) endImage = 1
  co_counter = 0

  sync all
  call sleep_(mod(selfImage,8)*3)
  if (selfImage == 1) then
     co_counter = 1
     print *, "  Start Image #", selfImage, "  counter=", co_counter
     if (numImages == 1) error stop "Only one . . ."
  else
     sync images( selfImage - 1)
     co_counter = co_counter[selfImage - 1] + 1
     print *, " A between Image #", selfImage, "  counter=", co_counter
     if (selfImage == endImage) error stop "That was enough"
     if (selfImage == numImages) print *, "  Final Image #", selfImage, "  counter=", co_counter
  end if
  if (selfImage < numImages) sync images( selfImage + 1)

end program error_stop_prog
