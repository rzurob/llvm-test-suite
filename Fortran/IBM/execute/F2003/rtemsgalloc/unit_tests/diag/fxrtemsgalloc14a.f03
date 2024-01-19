!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Oct. 31, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Providing file name and line number
!*                               when allocate or deallocate statements
!*                               fail at run-time. Customer requested
!*                               feature 305340.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*  REQUIRED RUNTIME OPTIONS   : None.
!*
!*  DESCRIPTION                : This diagnostic test, makes sure that
!*                               if a deallocate statement inside an
!*                               external procedure that appears in a
!*                               different file fails at runtime, correct
!*                               file name and line number is displayed.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
subroutine s1()

      real, allocatable :: rr(:,:)
      allocate(rr(0:12,1:13))
      allocate(rr(0:1,0:1))

end subroutine
