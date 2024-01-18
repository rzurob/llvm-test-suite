!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : cafsum.f
!*
!*  PROGRAMMER                 : Xing Xue
!*  DATE                       : July 31, 2009
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray access
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf95_r
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : functional testing of REAL*16 coarray
!*                               access.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program QCAFSUM
  implicit none
  real*16 :: caf0[*]
  SAVE caf0
  real*16 :: caf1(10)[*]
  SAVE caf1
  real*16 :: caf2(10,10)[*]
  SAVE caf2
  real*16 :: sum, sum0
  real*16 :: sum1(10)
  real*16 :: sum2(10, 10)
  integer :: me, num_imgs
  integer :: i, j

  me = this_image()
  num_imgs = num_images()

  caf0 = me
  caf1(1:10) = me
  caf2(1:10, 1:10) = me

  SYNC ALL

  if (me .eq. 1) then
     sum = 0
     sum0 = 0
     sum1(1:10) = 0
     sum2(1:10, 1:10) = 0
     do i=1, num_imgs
        sum = sum + i
        sum0 = sum0 + caf0[i] 
        sum1(1:10) = sum1(1:10) + caf1(1:10)[i] 
        sum2(1:10, 1:10) = sum2(1:10, 1:10) + caf2(1:10, 1:10)[i] 
     enddo
     if (sum0 .ne. sum) then
        error stop 1
     end if
     do i=1, 10
        if (sum1(i) .ne. sum) then
           error stop 2
        end if
     end do
     do i=1, 10
        do j=1, 10
           if (sum2(i,j) .ne. sum) then
               error stop 3
           end if
        end do
     end do
     print *,"sum0=",sum0
     print *,"sum1=",sum1
     print *,"sum2=",sum2
  endif
  
  SYNC ALL
  
end program QCAFSUM
