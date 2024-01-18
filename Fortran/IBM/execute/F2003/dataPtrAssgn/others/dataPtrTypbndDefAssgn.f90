!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrTypebndDefineAssgn.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!*  - type bound defined assignment a1 = a2;
!*   apply ptr assignment to ptr component of a1/a2 in defined assgn procedure
!*
!*  234567890123456789012345678901234567890123456789012345678901234567890
            module m

		type A
		    integer id
		end type

		type base
		    class(*), pointer :: p(:)
		    contains
			procedure :: assgnPtr
			generic :: assignment(=) => assgnPtr
		end type

		contains
    	            subroutine assgnPtr(a1, a2)
		 	class(base), intent(inout) :: a1
			type(base), intent(in) :: a2

			a1%p(lbound(a2%p,1):ubound(a2%p,1)/2)  =>  a2%p
			a1%p(ubound(a1%p,1) : )  =>  a1%p

		    end subroutine
	    end module

        program main
                use m

		type(base) :: a1
		class(base), target, allocatable :: a2

		type(A), target :: tar(20)

		tar = ( / (A(-i), i=1,20) /)

		allocate(a2, source = base( tar ))
		if ( .not. allocated(a2) ) stop 3
		if ( .not. associated(a2%p, tar) ) stop 5

		a1 = a2

		if ( .not. associated(a1%p) ) stop 7
		if ( lbound(a1%p,1) /= 10) stop 11
		if ( ubound(a1%p,1) /= 19) stop 13

		select type (x=>a1%p)
		    type is (A)
			print *, x%id
		    class default
			stop 21
		end select

        End program
