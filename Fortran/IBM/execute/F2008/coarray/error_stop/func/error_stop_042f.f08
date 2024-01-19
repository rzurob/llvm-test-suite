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
!*  DESCRIPTION                : ERROR STOP interrupting others images'
!*                               work in the middle
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program error_stop_prog
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
  print *, "   Begin of image #", selfImage
  print *, "   End of image #", selfImage
  error stop "Correct end"

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
     error stop "Correct end"
  else
     print *, "   Begin of image #", selfImage
     sync images(endImage)
     call sleep_(mod(selfImage,8)*3) ! wait some more
     do i=1, iter
        co_sum = co_sum + coarr_1(i) + selfImage+ coarr_1(i)[1]
     end do
     print *, "   End of image #", selfImage
  end if

  sync all   !<-To be removed when END PROGRAM implemented
end program error_stop_prog
