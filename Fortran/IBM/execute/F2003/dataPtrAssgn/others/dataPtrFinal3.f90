!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrFinal3.f 
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
!* - data-target is type-bound proc with returns array pointer 
!* - when data-pointer is deallocated, the target is finalized 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

	integer :: count = 0

	type A
	    contains
		final :: final1
	end type

	contains

	subroutine final1(a)
		type(A), intent(inout) :: a(:)
		count = count + 1
	end subroutine

end module

module n
	use m
	type base
	    type(A), pointer :: p(:)
	    contains
		procedure, nopass :: fun
	end type

	interface
	    function fun(arg)
		import base , A
		type(base), intent(in) :: arg
		type(A), pointer :: fun(:)
	    end function
	end interface
end module

module k
	use n
end module

    program main

        use k 

	type(base) :: b1, b2 

	allocate(b2%p(10))

	b1%p(1:) => b1%fun(b2)

	if ( .not. associated(b1%p) ) stop 12
	if ( lbound(b1%p,1) /= 1 ) stop 15
	if ( ubound(b1%p,1) /= 10 ) stop 17

        deallocate(b1%p)	

	if ( count /= 1 ) stop 1

    end program

	    function fun(arg)
		use n, only : base , A
		type(base), intent(in) :: arg
		type(A), pointer :: fun(:)

		allocate(fun(size(arg%p)), source= arg%p) 
	    end function

