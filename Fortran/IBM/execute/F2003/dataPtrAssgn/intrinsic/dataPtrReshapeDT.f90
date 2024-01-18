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
!* - data-pointer of derived-type, as arg of reshape
!* - data-target is tar1%b1%a1; tar1 and a1 are scalar of DTs, b1 is array of DT
!* - a1 has integer allocatable component
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type base
	integer, allocatable :: id
    end type

    type A
        type(base) :: a1
    end type

    type :: B
        type(A) :: b1(10)
    end type
end module

    program main

        use m

        class(base), pointer :: p1(:)
        type(B), target :: tar1
        type(base), allocatable :: res(:,:)

        tar1%b1 = (/ ( A(base(i)),i= 1, 10 ) /)

        p1(-1:8) => tar1%b1%a1

        if ( .not. associated(p1, tar1%b1%a1)) stop 11
        if (lbound(p1,1) /= -1 ) stop 13
        if (ubound(p1,1) /= 8 ) stop 15

	print *, (/ (p1(i)%id, i= -1,8)/)

	res = reshape(p1,(/2, 5/) )

	print *, shape(res)

	do i = 1, 2
	    do j = 1, 5
                print *, (/ (res(i,j)%id ) /)
	    enddo
	end do

    end program
