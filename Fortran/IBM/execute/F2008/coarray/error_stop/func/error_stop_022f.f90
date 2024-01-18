!#######################################################################
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
!*  DESCRIPTION                : Basic case for ERROR STOP interrupting the work
!*                               internal function
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program error_stop_prog
  implicit none
  integer, parameter :: iter = 10000
  integer :: selfImage, endImage, numImages
  integer, save :: coarr_1(iter)[*]
  integer :: i, co_sum
  integer :: f_ret

  selfImage = this_image()
  numImages = num_images()
  endImage  = numImages/2
  if (endImage < 1) endImage = 1
  co_sum = 0

  do i = 1, iter
    coarr_1(i)  = i + selfImage
  end do

  sync all
  call sleep_(mod(selfImage,8)*3)
  if (selfImage == endImage) then
     f_ret = inter_f_es()
     print *, "  Ret code:", f_ret
  else
     do i=1, iter
        co_sum = co_sum + coarr_1(i) + selfImage
     end do
     print *, "   . . . Inside NON-end image #", selfImage
  end if
  sync all

!  print *, selfImage, "  co_sum=", co_sum
   if (co_sum == (((iter*iter-1*1+iter+1)/2)+2*iter*selfImage) ) then
      print *, "The sum is correct. Error since should note come here #", selfImage
      error stop "Bad end"
   end if

  sync all   !<-To be removed when END PROGRAM implemented
  contains
    integer function inter_f_es()
      print *, "   . . . Inside end image #", selfImage
      if (num_images() > 0)  error stop "Correct end"
      inter_f_es = this_image()
    end function inter_f_es
end program error_stop_prog
