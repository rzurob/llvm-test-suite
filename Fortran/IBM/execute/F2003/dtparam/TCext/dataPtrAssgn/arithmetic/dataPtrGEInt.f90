! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrGEInt.f
! opt variations: -qnok -ql

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrGEInt.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-target is type bound procedure pointer
!* - the part of data-ptr is redfined by itself
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type base(k1)    ! (4)
        integer, kind :: k1
    end type
end module

module n
    use m

    type, extends(base) :: A(k2)    ! (4,8)
	integer, kind        :: k2
	integer(k2), pointer :: p(:,:,:)
    end type

    type, extends(base) :: B    ! (4)
	procedure(func), nopass, pointer :: pp => null()
    end type

    interface
	function func(arg)
	    integer*8, pointer :: func(:)
	    integer :: arg(:)
	end function
    end interface
end module

program main

    use n

    type(B(4)) :: b1
    type(A(4,8)), allocatable :: a1

    b1%pp => func

    a1 = A(4,8)(null())

    a1%p(1:2,3:4,5:7) => b1%pp( [-1,0,1,2,3,4,5,6,7,8,9,10])

    if ( .not. associated(a1%p)) stop 1
    if ( any (lbound(a1%p) .ne. (/1,3,5/))) stop 2
    if ( any (ubound(a1%p) .ne. (/2,4,7/))) stop 3

    if ( any(a1%p(2,3,:) .ge. (/0,4,8 /)) ) then
    	a1%p(2,3,:) = a1%p(2,3, 7:5:-1)
    endif

    print *, a1%p

end program

	function func(arg)
	    integer*8, pointer :: func(:)
	    integer :: arg(:)

	    allocate(func(size(arg)), source = int(arg,8))
	end function
