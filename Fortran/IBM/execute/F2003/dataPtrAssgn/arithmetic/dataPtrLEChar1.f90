!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrLEChar1.f 
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
!* - data-target is an assumed-size array section of type character(*)
!* - data-ptr is of type class(*) 
!* 
!234567890123456789012345678901234567890123456789012345678901234567890

    character(:), target, allocatable :: arr(:)
    class(*), pointer :: ptr(:)

    allocate(arr(4), source=(/'ibm-xlC ','ibm-xlc ','ibm-XLF ','COMPILER' /))

    call sub(arr)

    if ( .not. associated(ptr, arr(4:2:-1))) stop 2
    if ( lbound(ptr, 1) /= 1 ) stop 5
    if ( ubound(ptr, 1) /= 3 ) stop 8 

    select type (ptr)
	type is (character(*))
	    print *, ptr
 	    print *, ptr .LE. (/'compiler','ibm-xlf ','ibm-xlc ' /)
        class default
	    stop 21
    end select
    contains

    subroutine sub(arg)
	character(*), target :: arg(*)

	ptr(lbound(arg,1):) => arg(4:2:-1)

    end subroutine 

end program

