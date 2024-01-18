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
!*  DESCRIPTION                : This is the file to be included in
!*                               fxrtemsgalloc12.f
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      integer, allocatable :: x(:)
      allocate(x(0:10))
      allocate(x(0:10))


