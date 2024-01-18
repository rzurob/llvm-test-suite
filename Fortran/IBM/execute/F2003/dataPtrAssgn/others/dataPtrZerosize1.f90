!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrZerosize1.f 
!*
!*  PROGRAMMER                 : Michelle Zhang
!*  DATE                       : Aug 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
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

    if ( .not. associated(ptr) ) stop 5 
    if (lbound(ptr,1) /= 1) stop 7 
    if (ubound(ptr,1) /= 0) stop 9 

End program
