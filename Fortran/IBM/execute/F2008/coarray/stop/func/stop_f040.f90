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
!*  DESCRIPTION                : STOP initiating normal termination of
!*                               execution of ordered image work
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program stop_prog
  implicit none
  integer       :: selfImage, endImage, numImages
  integer, save :: co_counter[*]
  integer       :: i

  selfImage = this_image()
  numImages = num_images()
  endImage  = numImages/2
  if (endImage < 1) endImage = 1
  co_counter = 0

  if (numImages == 1) then
     print *, "  Start Image #", selfImage, "  counter=", co_counter
     print *, "  Final Image #", selfImage, "  counter=", co_counter
     print *, "  Image with STOP statement:",  selfImage, "  counter=", co_counter
     print *, " STOP That was enough, NOT"
     error stop "Only one . . ."
  end if

  sync all
  if (selfImage == 1) then
     co_counter = 1
     print *, "  Start Image #", selfImage, "  counter=", co_counter
     print *, "      Inside Ordered Image #", selfImage, "  counter=", co_counter
  else
     ! some sleep if not first image
     call sleep_(mod(selfImage,8)*3 + 4)
     sync images(selfImage - 1)
     co_counter = co_counter[selfImage - 1] + 1
     print *, "      Inside Ordered Image #", selfImage, "  counter=", co_counter
     if (selfImage == endImage)  then
        print *, "  Image with STOP statement:",  selfImage, "  counter=", co_counter
        ! using ERROR STOP until STAT_STOPPED_IMAGE got implemented
        error stop "That was enough, NOT"
     end if
     if (selfImage == numImages) print *, "  Final Image #", selfImage, "  counter=", co_counter
  end if
  if (selfImage < numImages) sync images( selfImage + 1)

  if (this_image() == num_images() .and. selfImage /= co_counter) then
     print *, "Error: Counter is incorrect in image #", selfImage, "  counter=", co_counter
     error stop
  end if

end program stop_prog
