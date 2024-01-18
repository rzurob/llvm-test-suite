! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/dataPtrAssgn/others/dataPtrFinal3.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrFinal3.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-target is type-bound proc with returns array pointer
!* - when data-pointer is deallocated, the target is finalized
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

	integer :: count = 0

	type A(k1)    ! (4)
	    integer, kind :: k1
	    contains
		final :: final1
	end type

	contains

	subroutine final1(a)
		type(A(4)), intent(inout) :: a(:)
		count = count + 1
	end subroutine

end module

module n
	use m
	type base(k2)    ! (4)
	    integer, kind        :: k2
	    type(A(k2)), pointer :: p(:)
	    contains
		procedure, nopass :: fun
	end type

	interface
	    function fun(arg)
		import base , A
		type(base(4)), intent(in) :: arg
		type(A(4)), pointer :: fun(:)
	    end function
	end interface
end module

module k
	use n
end module

    program main

        use k

	type(base(4)) :: b1, b2

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
		type(base(4)), intent(in) :: arg
		type(A(4)), pointer :: fun(:)

		allocate(fun(size(arg%p)), source= arg%p)
	    end function

