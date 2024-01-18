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
!* - data-tar is expr involving type bound defined operator
!*
!234567890123456789012345678901234567890123456789012345678901234567890

           module m
                type base
                    class(*), pointer :: p(:)
                    contains
                        procedure :: add
                        generic :: operator(+) => add
                end type

                contains
                    function add(a1, a2)
                        class(base), target, intent(in) :: a1
                        type(base), intent(in) :: a2
			integer, pointer :: add(:)

			if ( .not. associated(a1%p)) stop 11
			if ( .not. associated(a2%p)) stop 12

		        select type(x => a1%p)
			    type is (integer)
				select type(y => a2%p)
				    type is (integer)
			  	        allocate(add(size(y)), source=x+y)
				    class default
					stop 13
				end select
			    class default
				stop 15
			end select
                    end function
            end module


program main

	use m
	type(base) b1, b2, b3

	allocate(b1%p(10),  source = [ 0,2,4,1,5,8,3,7,6,9 ])

	b2%p(3:12) => b1%p(10:1:-1)

        if ( .not. associated(b2%p, b1%p(10:1:-1))) stop 1
	if ( lbound(b2%p, 1) /= 3) stop 2
	if ( ubound(b2%p, 1) /= 12) stop 3

	b2%p(9:) => b1+b2

        if ( .not. associated(b2%p)) stop 21
	if ( lbound(b2%p, 1) /= 9) stop 22
	if ( ubound(b2%p, 1) /= 18) stop 23

	select type(x=>b2%p)
	    type is (integer)
		if ( any(x .ne. (/ [9,8,11,4,13], 13,4,11,8,9/)))stop 25
	    class default
		stop 20
	end select
    end program

