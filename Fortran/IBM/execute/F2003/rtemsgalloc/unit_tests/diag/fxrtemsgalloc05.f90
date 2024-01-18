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
!*                               if deallocate statement fails at runtime
!*                               and the errloc runtime option is set to "yes"
!*                               line number and file name appear on
!*                               the error device before the message text.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      integer, allocatable :: a
      call setrteopts("errloc=yes")
      deallocate(a)
      end
