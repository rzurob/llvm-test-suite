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
!*  DESCRIPTION                : This tests the functionality of the intrinsic when used
!*                               in initialization expressions.
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      logical :: a1 = IS_IOSTAT_END(-1)
      logical :: a2 = IS_IOSTAT_END(-2)
      logical :: a3 = IS_IOSTAT_END(-3)
      logical :: a4 = IS_IOSTAT_END(-4)
      logical :: a5 = IS_IOSTAT_END(0)
      logical :: a6 = IS_IOSTAT_END(1)

      logical :: b1 = IS_IOSTAT_EOR(-1)
      logical :: b2 = IS_IOSTAT_EOR(-2)
      logical :: b3 = IS_IOSTAT_EOR(-3)
      logical :: b4 = IS_IOSTAT_EOR(-4)
      logical :: b5 = IS_IOSTAT_EOR(0)
      logical :: b6 = IS_IOSTAT_EOR(1)

      integer, parameter :: size = 7
      integer, parameter, dimension(size) :: arg = (/-4,-3,-2,-1,0,1,2/)
      logical, dimension(size) :: aa1 = IS_IOSTAT_END(arg)
      logical, dimension(size) :: bb1 = IS_IOSTAT_EOR(arg)
      logical, dimension(size) :: aa2 = IS_IOSTAT_END(arg-1)
      logical, dimension(size) :: bb2 = IS_IOSTAT_EOR(arg-1)

      integer, parameter, dimension(2,size) :: arg2 =                   &
     &     ( RESHAPE( (/-4,-4,-2,-1,0,1,2,-3,-3,-3,-1,-2,-4,-4/),       &
     &                (/2,size/) ) )
      logical, dimension(2,size) :: aa3 = IS_IOSTAT_END(arg2)
      logical, dimension(2,size) :: bb3 = IS_IOSTAT_EOR(arg2)
      logical, dimension(2,size) :: aa4 = IS_IOSTAT_END(arg2-1)
      logical, dimension(2,size) :: bb4 = IS_IOSTAT_EOR(arg2-1)

      write(*,*) a1, a2, a3, a4, a5, a6
      write(*,*) b1, b2, b3, b4, b5, b6
      write(*,*)
      write(*,*) aa1
      write(*,*) bb1
      write(*,*) aa2
      write(*,*) bb2
      write(*,*) aa3
      write(*,*) bb3
      write(*,*) aa4
      write(*,*) bb4

      end
