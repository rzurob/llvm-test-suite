!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrNullDT.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-pointer of class(*), as arg of null
!* - the left part-name of data-target is an array of derived type
!* - the right part-name of data-target is a component of another derived type
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type base
	character(len=1), allocatable :: ch
    end type

    type A
        type(base) :: a1
    end type

    type :: B
        type(A) :: b1
    end type
end module

    program main

        use m

        class(*), pointer :: p1(:)
        type(B), target :: tar1(20)
        type(base), allocatable :: res(:,:,:)

        tar1 = (/ ( B(A(base(achar(i)))),i=66, 85 ) /)

	do i = 1, 20
	    print *,  tar1(i)%b1%a1%ch
	end do

	! print *, (/ (tar1(i)%b1%a1%ch, i= 11,30)/)

        p1(11:) => tar1%b1%a1

        if ( .not. associated(p1, tar1%b1%a1)) stop 11
        if (lbound(p1,1) /= 11 ) stop 13
        if (ubound(p1,1) /= 30 ) stop 15

	select type (p1)
	    type is (base)
	         print *, (/ (p1(i)%ch, i= 11,30)/)
	    class default
		stop 25
	end select

	if ( associated( null(p1) )) stop 35

    end program
