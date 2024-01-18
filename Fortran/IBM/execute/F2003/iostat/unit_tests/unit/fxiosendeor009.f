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
!*                               to the intrinsic are constat arrays of integers
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer,parameter::arg1(10)=(/-1,-2,-3,-4,-5,-6,10,100,-100,0/)

      write(*,*) IS_IOSTAT_END( arg1 )
      write(*,*) IS_IOSTAT_END( (/-5,-4,-3,-2,-1,0,1,2,3,4,5/) )
      write(*,*)
      write(*,*) IS_IOSTAT_EOR( arg1 )
      write(*,*) IS_IOSTAT_EOR( (/-5,-4,-3,-2,-1,0,1,2,3,4,5/) )

      end
