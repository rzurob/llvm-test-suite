!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :C724
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  integer, pointer :: ptr1(:)


  !function returns integer, not okay
  ptr1(5:)=>func1()
  ptr1(5:15)=>func1()

  contains


    function func1()
      integer, target :: func1

      func1=0
    end function
end

