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
!* - data-target is type-bound proc which returns pointer array
!* - lb of data-pointer is the value of kind parameter of DT
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type dtp(k1)
	integer, kind :: k1
    end type

    type base
    end type

    type, extends(base) :: child
	integer :: index
	contains
	    procedure, nopass :: typBnd1
    end type

    contains
	function typBnd1(a)
	    type(child), pointer :: typBnd1(:)
	    type(child), intent(in) :: a(:)

	    allocate(typBnd1(size(a)), source = a)
	end function

end module

module n
    use m

    type(dtp(1)) :: dtParam
    class(base), pointer :: p_b(:)
end module

    program main
	use n

	class(base), allocatable :: b1

	allocate(child::b1)

        select type (b1)
	    type is (child)
        	p_b(dtParam%k1:kind(b1%index)*4) =>  &
                       b1%typBnd1((/ ( child(i), i = 1, 20 ) /))
	        if ( .not. associated(p_b) ) stop 11
	        if ( lbound(p_b,1) /= 1 ) stop 15
	        if ( ubound(p_b,1) /= 16 ) stop 19
	    class default
		stop 21
	end select

	print *, shape(p_b)

        select type (p_b)
	    type is (child)
		p_b%index = p_b(16:1:-1)%index
		print *, p_b%index
	    class default
		stop 21
	end select

    end program
