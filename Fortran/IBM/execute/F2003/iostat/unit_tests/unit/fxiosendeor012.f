!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxiosendeor012.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 19, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This tests the functionality of the intrinsic when args
!*                               to the intrinsic are arrays sections.
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer, parameter :: size = 10, size2 = 43
      integer :: b = 1, i
      integer, dimension(size2*2+1) :: tmpArr
      logical, dimension(size2*2+1) :: tmpRes
      integer :: arr(size+2)  = (/-6,-5,-4,-3,-2,-1,0,1,2,3,4,5/)
      logical, parameter ::                                             &
     &  res1end(size) = (/.false., .false., .false., .true.,            &
     &                           .true., .false., .false., .false.,     &
     &                           .false., .false./)
      logical, parameter ::                                             &
     &   res2end(size) = (/.false., .false., .false., .false.,          &
     &                           .true., .true., .false., .false.,      &
     &                           .false., .false./)

      logical, parameter ::                                             &
     &  res1eor(size) = (/.false., .true., .false., .false.,            &
     &                           .false., .false., .false., .false.,    &
     &                           .false., .false./)
      logical, parameter ::                                             &
     &  res2eor(size) = (/.false., .false., .true., .false.,            &
     &                           .false., .false., .false., .false.,    &
     &                           .false., .false./)
      logical :: results(size)

! IS_IOSTAT_END:
      results = ( is_iostat_end(arr(2:size+1)) .eqv. res1end )
      do i=1, size, 1
         if( .not. results(i)  ) then
            error stop 1
         endif
      end do
      results = ( is_iostat_end(arr(2:size+1)-1) .eqv. res2end )
      do i=1, size, 1
         if( .not. results(i) ) then
            error stop 2
         endif
      end do

! IS_IOSTAT_EOR
      results = ( is_iostat_eor(arr(2:size+1)) .eqv. res1eor )
      do i=1, size, 1
         if( .not. results(i)  ) then
            error stop 3
         endif
      end do
      results = ( is_iostat_eor(arr(2:size+1)-1) .eqv. res2eor )
      do i=1, size, 1
         if( .not. results(i) ) then
            error stop 4
         endif
      end do


! TEST A WIDER RANGE OF NUMBERS

  ! IS_IOSTAT_END:

      do i=-size2, size2
         tmpArr(i+size2+1) = i ! generate some numbers
      enddo

      tmpRes(3:size2*2+1) = is_iostat_end( tmpArr(3:size2*2+1) )
      do i=3, size2*2+1
         if( ( tmpArr(i) .eq. -2 .or. tmpArr(i) .eq. -1 ) .and.               &
     &       .not. tmpRes(i) ) then
            error stop 5
         else if ( (tmpArr(i) .ne. -2) .and. (tmpArr(i) .ne. -1) .and.        &
     &             tmpRes(i) )then
            error stop 6
         endif
      end do

  ! IS_IOSTAT_EOR:

      do i=-size2, size2
         tmpArr(i+size2+1) = i ! generate some numbers
      enddo
      tmpRes(3:size2*2+1) = is_iostat_eor( tmpArr(3:size2*2+1) )
      do i=3, size2*2+1
         if( tmpArr(i) .eq. -4 .and. .not. tmpRes(i) ) then
            error stop 7
         else if ( tmpArr(i) .ne. -4 .and. tmpRes(i) )then
            error stop 8
         endif
      end do

      end
