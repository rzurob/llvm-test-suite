!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxiosendeor013.f
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
!*                               to the intrinsic are arrays sections and the intrinsics
!*                               are being used in initialization expressions.
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer i
      integer, parameter :: arr( 7 ) = (/-3,-4,-2,-1,0,1,2/)
      logical, parameter :: cmpend( 4 ) =                               &
     & (/.false., .true., .true., .false./)
      logical, parameter :: cmpeor( 4 ) =                               &
     & (/.true., .false., .false., .false./)

      logical :: cmp(4)

      logical :: res1end(4) = IS_IOSTAT_END(arr(2:5))

      logical :: res1eor(4) = IS_IOSTAT_EOR(arr(2:5))

      cmp = cmpend .eqv. res1end
      do i = 1, 4
         if( .not. cmp(i) ) then
            error stop 1
         endif
      enddo
      cmp = cmpeor .eqv. res1eor
      do i = 1, 4
         if( .not. cmp(i) ) then
            error stop 2
         endif
      enddo

      end
