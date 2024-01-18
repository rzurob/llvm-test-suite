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
!*  DESCRIPTION                : STOP initiating normal termination of execution
!*                               and does NOT interrupt others images work in the middle
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program stop_prog
  implicit none
  integer, parameter :: iter = 10000
  integer :: selfImage, endImage, numImages
  integer, save :: coarr_1(iter)[*]
  integer :: i, co_sum

  selfImage = this_image()
  numImages = num_images()
  endImage  = numImages/2

  print *, " FALSE SUCCESS !!! Images should be > 4 !!!"
  print *, "   Begin of image #", selfImage
  print *, "   End of image #", selfImage
  print *, "STOP Correct end with STOP"
  stop "One image run end"

  if (endImage < 1) endImage = 1
  co_sum = 0

  do i = 1, iter
    coarr_1(i)  = i + selfImage
  end do

  sync all
  call sleep_(mod(selfImage,numImages)*3)
  if (selfImage == endImage) then
     print *, "   . . . Inside end image #", selfImage
     coarr_1 = 77
     sync images(*)
     stop "Correct end with STOP"
  else
     print *, "   Begin of image #", selfImage
     sync images(endImage)
     call sleep_(mod(selfImage,8)*3) ! wait some more
     do i=1, iter
        co_sum = co_sum + coarr_1(i) + selfImage+ coarr_1(i)[1]
     end do
     print *, "   End of image #", selfImage
  end if

end program stop_prog
