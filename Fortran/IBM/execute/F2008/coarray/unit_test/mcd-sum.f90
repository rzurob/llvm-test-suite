!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : mcd-sum.f
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
!*  DESCRIPTION                : functional testing of coarray
!*                               access. This test case was
!*                               originally from Rice Univ. Changes
!*                               have been made to enhance it.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program MCD_SUM
  implicit none

  print *,'MCD_SUM_save'
  call MCD_SUM_save
  !print *,'MCD_SUM_common'
  !call MCD_SUM_common
  !print *,'subroutineDriver'
  !call subroutineDriver

end program MCD_SUM



subroutine MCD_SUM_save
  implicit none

  integer :: caf0[2,3,*]
  SAVE caf0
  integer :: caf1(10)[2,3,*]
  SAVE caf1
  integer :: caf2(10,10)[2,3,*]
  SAVE caf2
  integer :: sum, sum0
  integer :: sum1(10)
  integer :: sum2(10, 10)
  integer :: me, num_imgs, me1, me2, me3, me0
  integer :: i, i0, j

  me = this_image()
  me0 = me-1
  num_imgs = num_images()

  caf0 = me
  caf1(1:10)[mod(me0,2)+1, mod(me0/2,3)+1,me0/(2*3)+1] = me
  caf2(1:10, 1:10) = me

  SYNC ALL

  if (me .eq. 1) then
     sum = 0
     sum0 = 0
     sum1(1:10) = 0
     sum2(1:10, 1:10) = 0
     do i=1, num_imgs
        i0 = i-1
        sum = sum + i
        sum0 = sum0 + caf0[mod(i0,2)+1, mod(i0/2,3)+1,i0/(2*3)+1]
        sum1(1:10) = sum1(1:10) +  &
             caf1(1:10)[mod(i0,2)+1, mod(i0/2,3)+1,i0/(2*3)+1]
        sum2(1:10, 1:10) = sum2(1:10, 1:10) +  &
             caf2(1:10, 1:10)[mod(i0,2)+1, mod(i0/2,3)+1,i0/(2*3)+1]
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
end subroutine MCD_SUM_save


!subroutine MCD_SUM_common
!  implicit none
!
!  integer :: caf0[2,3,*]
!  integer :: caf1(10)[2,3,*]
!  integer :: caf2(10,10)[2,3,*]
!  common /MCD_SUM_CB/ caf0, caf1, caf2
!  integer :: sum0
!  integer :: sum1(10)
!  integer :: sum2(10, 10)
!  integer :: me, num_imgs, me1, me2, me3, me0
!  integer :: i, i0
!
!  me = this_image()
!  me0 = me-1
!  num_imgs = num_images()
!
!  caf0 = me
!  caf1(1:10)[mod(me0,2)+1, mod(me0/2,3)+1,me0/(2*3)+1] = me
!  caf2(1:10, 1:10) = me
!
!  SYNC ALL
!
!  if (me .eq. 1) then
!     sum0 = 0
!     sum1(1:10) = 0
!     sum2(1:10, 1:10) = 0
!     do i=1, num_imgs
!        i0 = i-1
!        sum0 = sum0 + caf0[mod(i0,2)+1, mod(i0/2,3)+1,i0/(2*3)+1]
!        sum1(1:10) = sum1(1:10) +  &
!             caf1(1:10)[mod(i0,2)+1, mod(i0/2,3)+1,i0/(2*3)+1]
!        sum2(1:10, 1:10) = sum2(1:10, 1:10) +  &
!             caf2(1:10, 1:10)[mod(i0,2)+1, mod(i0/2,3)+1,i0/(2*3)+1]
!     enddo
!     print *,"sum0=",sum0
!     print *,"sum1=",sum1
!     print *,"sum2=",sum2
!  endif
!  
!  SYNC ALL
!end subroutine MCD_SUM_common


!subroutine subroutineDriver()
!  implicit none
!  integer :: caf0[2,3,*]
!  integer :: caf1(10)[*]
!  integer :: caf2(10,10)[2,3,*]
! 
!  interface 
!     subroutine initCoArrays(caf0, caf1, caf2) 
!       integer :: caf0[1,1,*]
!       integer :: caf1(1)[*]
!       integer :: caf2(1,1)[1,1,*]
!     end subroutine initCoArrays
!
!     subroutine sumCoArrays(caf0, caf1, caf2) 
!       integer :: caf0[2,3,*]
!       integer :: caf1(1)[*]
!       integer :: caf2(1,1)[2,3,*]
!     end subroutine sumCoArrays
!
!     subroutine sumCoArraysVarCoSpace(   &
!          caf0, caf0x, caf0y, &
!          caf1, caf1x, caf1y, &
!          caf2, caf2x, caf2y, n, m) 
!       integer caf0x, caf0y
!       integer caf1x, caf1y
!       integer caf2x, caf2y
!       integer :: caf0[1,1,*]
!       integer :: caf1(1)[*]
!       integer :: caf2(1,1)[1,1,*]
!       integer n, m
!     end subroutine sumCoArraysVarCoSpace
!
!  end interface
!
!  SYNC ALL
!  call initCoArrays(caf0, caf1, caf2)
!  SYNC ALL
!  call sumCoArrays(caf0, caf1, caf2)
!  SYNC ALL
!  call sumCoArraysVarCoSpace(caf0, 2, 3, caf1, 2, 3, caf2, 2, 3, 10, 5)
!  SYNC ALL
!
!end subroutine subroutineDriver
!
!
!subroutine initCoArrays(caf0, caf1, caf2) 
!  implicit none
!  integer :: caf0[2,3,*]
!  integer :: caf1(10)[2,3,*]
!  integer :: caf2(10,10)[2,3,*]
!  
!  integer :: me, num_imgs, me1, me2, me3, me0
!  
!  me = this_image()
!  me0 = me-1
!  num_imgs = num_images()
!  
!  caf0 = me
!  caf1(1:10)[mod(me0,2)+1, mod(me0/2,3)+1,me0/(2*3)+1] = me
!  caf2(1:10, 1:10) = me
!  
!end subroutine initCoArrays
!
!subroutine sumCoArrays(caf0, caf1, caf2)
!  implicit none
!  integer :: caf0[2,3,*]
!  integer :: caf1(10)[2,3,*]
!  integer :: caf2(10,10)[2,3,*]
!  integer :: sum0
!  integer :: sum1(10)
!  integer :: sum2(10, 10)
!  integer :: me, num_imgs, me1, me2, me3, me0
!  integer :: i, i0
!
!!  me = this_image()
!  me0 = me-1
!  num_imgs = num_images()
!
!  if (me .eq. 1) then
!     sum0 = 0
!     sum1(1:10) = 0
!     sum2(1:10, 1:10) = 0
!     do i=1, num_imgs
!        i0 = i-1
!        sum0 = sum0 + caf0[mod(i0,2)+1, mod(i0/2,3)+1,i0/(2*3)+1]
!        sum1(1:10) = sum1(1:10) +  &
!             caf1(1:10)[mod(i0,2)+1, mod(i0/2,3)+1,i0/(2*3)+1]
!        sum2(1:10, 1:10) = sum2(1:10, 1:10) +  &
!             caf2(1:10, 1:10)[mod(i0,2)+1, mod(i0/2,3)+1,i0/(2*3)+1]
!     enddo
!     print *,"sum0=",sum0
!     print *,"sum1=",sum1
!     print *,"sum2=",sum2
!  endif
!  
!end subroutine sumCoArrays
!
!
!subroutine sumCoArraysVarCoSpace(caf0, caf0x, caf0y, &
!                                 caf1, caf1x, caf1y, &
!                                 caf2, caf2x, caf2y, n, m)
!  implicit none
!  integer caf0x, caf0y
!  integer caf1x, caf1y
!  integer caf2x, caf2y
!  integer :: caf0[caf0x, caf0y,*]
!  integer :: caf1(n)[caf1x, caf1y,*]
!  integer :: caf2(2*m,m+m)[caf2x, caf2y,*]
!  integer n, m
!  integer :: sum0
!  integer :: sum1(n)
!  integer :: sum2(2*m, m+m)
!  integer :: me, num_imgs, me1, me2, me3, me0
!  integer :: i, i0
!
!  me = this_image()
!  me0 = me-1
!  num_imgs = num_images()
!
!  if (me .eq. 1) then
!     sum0 = 0
!     sum1(1:n) = 0
!     sum2(1:(2*m), 1:(m+m)) = 0
!     do i=1, num_imgs
!        i0 = i-1
!        sum0 = sum0 + caf0[mod(i0,caf0x)+1, mod(i0/caf0x,caf0y)+1,i0/(caf0x*caf0y)+1]
!        sum1(1:n) = sum1(1:n) +  &
!             caf1(1:n)[mod(i0,caf1x)+1, mod(i0/caf1x,caf1y)+1,i0/(caf1x*caf1y)+1]
!        sum2(1:(2*m), 1:(m+m)) = sum2(1:(2*m), 1:(m+m)) +  &
!             caf2(1:(2*m), 1:(m+m))[mod(i0,caf2x)+1, mod(i0/caf2x,caf2y)+1,i0/(caf2x*caf2y)+1]
!     enddo
!     print *,"sum0=",sum0
!     print *,"sum1=",sum1
!     print *,"sum2=",sum2
!  endif
!  
!end subroutine sumCoArraysVarCoSpace
