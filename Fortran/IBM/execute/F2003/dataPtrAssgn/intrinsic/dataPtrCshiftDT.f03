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
!* - data-pointer of derived type, as arg of cshift
!* - the left part-name of data-target is an array of derived type
!* - the right part-name of data-target is scalar component
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base
        integer id
    end type

    type, extends(base) :: A
	logical*8, allocatable :: ll
    end type

end module

    program main

        use m

        class(base), pointer :: p1(:,:,:)
        type(A), target :: tar1(4,4,4)
        type(base), allocatable :: res(:,:,:)

        tar1 = reshape( (/ ( A(i,logical(mod(i,2) == 0,8)) ,i=1,64 ) /) , (/4,4,4/))

        p1(2:,1:,3:) => tar1(1:3,2:3,2:4)%base

        if ( .not. associated(p1)) error stop 11
        if ( any (lbound(p1) .ne. (/2,1,3/) )) error stop 13
        if ( any (ubound(p1) .ne. (/4,2,5/) )) error stop 17

	select type (p1)
	    type is (base)
		print *, p1%id
	    class default
		stop 19
	end select

        res = cshift(p1, reshape((/-1,1,1,0,-1,0/)  ,(/3,2 /) ) , dim = 3)
	print *, shape(res)
	print *, res%id

    end program