!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data_ptr is of type class(*), becomes zero-size array pointer
!* - data-tar is zero size array pointer
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    class(*), pointer :: ptr(:)
end module


program main
    use m

    complex(4), target :: tar(6,4,2)

    ptr(2:0) => tar(:,4,1)

    ptr(size(ptr):) => ptr

    if ( .not. associated(ptr) ) error stop 5
    if (lbound(ptr,1) /= 1) error stop 7
    if (ubound(ptr,1) /= 0) error stop 9

End program