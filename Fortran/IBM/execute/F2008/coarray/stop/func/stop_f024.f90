!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : F2008/coarray/stop/func/stop_f024.f
!*  TYPE                       : Functional test
!*  FEATURE                    : #351605.31 CAF - STOP statement
!*
!*  PROGRAMMER                 : Grigor Nikolov
!*  DATE                       : 19 Oct 2010
!*  ORIGIN                     : XLF Test -  IBM Toronto Lab
!*
!*  REQUIRED COMPILER OPTIONS  : 
!*  DEPENDENCIES               :
!*
!*  DESCRIPTION                : Basic case for STOP initiating normal termination
!*                               external function
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program stop_prog
  implicit none
  integer, parameter :: iter = 10000
  integer :: selfImage, endImage, numImages
  integer, save :: coarr_1(iter)[*]
  integer :: i, co_sum
  integer :: f_ret

   interface
      integer function extrn_f_es(oImage)
        integer :: oImage    
      end function extrn_f_es
   end interface

  selfImage = this_image()
  numImages = num_images()
  if (numImages < 2) then
    print *, " FALSE SUCCESS !!! Images should be > 2 !!!"
    print *, "The main stop"
    print *, "Not main stop"
    stop
  end if
  endImage  = numImages/2
  if (endImage < 1) endImage = 1
  co_sum = 0

  do i = 1, iter
    coarr_1(i)  = i + selfImage
  end do

  sync all
  call sleep_(mod(selfImage,8)*3)
  if (selfImage == endImage) then
     f_ret = extrn_f_es(selfImage) 
     print *, "  Ret code:", f_ret
  else
     do i=1, iter
        co_sum = co_sum + coarr_1(i) + selfImage
     end do
     print *, "   . . . Inside NON-end image #", selfImage
     stop "Not main stop"
  end if
  sync all

!  print *, selfImage, "  co_sum=", co_sum
   if (co_sum == (((iter*iter-1*1+iter+1)/2)+2*iter*selfImage) ) then
      print *, "The sum is correct. Error since should note come here #", selfImage
      error stop "Bad end"
   end if

end program stop_prog

integer function extrn_f_es(oImage)
  implicit none
  integer :: oImage    

  print *, "   . . . Inside end image #", oImage
  if (num_images() > 0)  stop "The main stop"
  extrn_f_es = this_image()
end function extrn_f_es
