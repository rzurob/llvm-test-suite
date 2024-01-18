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
!*                               in an implied-do loop, in an initialization expression.
!*
!*  IMPORTANT CONSIDERATION    : THIS TEST CASE WILL FAIL BECAUSE OF A KNOWN BUG, FOR
!*                               WHICH I HAVE OPENED A DEFECT (308489). UNTIL THIS DEFECT
!*                               IS FIXED, THE TESTCASE WILL FAIL.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer :: i
      integer, parameter :: input(6) = (/-1,-2,-3,-4,0,1/)
      logical, dimension(6) :: idlend =                                 &
     & (/ ( is_iostat_end(input(i)), i=1, 6) /)
      logical, dimension(6) :: idleor =                                 &
     & (/ ( is_iostat_eor(input(i)), i=1, 6) /)

      write(*,*) idlend
      write(*,*) idleor

      end
