!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug. 19, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : is_iostat_end and is_iostat_eor intrinsics
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This tests the functionality of the intrinsic when args
!*                               to the intrinsic are allocatable arrays.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer, allocatable, dimension(:) :: alarr

      allocate( alarr(2:7) )
      alarr(2:7) = (/-4,-3,-2,-1,0,1/)
      write(*,*) IS_IOSTAT_END(alarr)
      write(*,*) IS_IOSTAT_EOR(alarr)

      deallocate(alarr)

      allocate( alarr(1:8) )
      alarr(1:8) = (/-4,-3,-2,-1,0,1,-4,-2/)
      write(*,*) IS_IOSTAT_END(alarr)
      write(*,*) IS_IOSTAT_EOR(alarr)

      end
