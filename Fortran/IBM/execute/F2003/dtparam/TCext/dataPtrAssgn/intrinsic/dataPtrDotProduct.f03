! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrDotProduct.f
! opt variations: -qnol

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
!* - data-target is type bound proc defined by entry function
!* - data-pointer is entry function result
!* - data-target is the component of a dummy arg of type bound proc
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(n1,k1)    ! (20,4)
	integer, kind        :: k1
	integer, len         :: n1
	integer(k1), pointer :: p(:)
	contains
	    procedure, nopass :: ent
	    procedure, nopass :: get_val
    end type

    contains
	function get_val(a)
	    type(base(*,4)), intent(in) :: a
	    integer, pointer :: res(:), get_val(:)

	    get_val(0:) => a%p(ubound(a%p,1):lbound(a%p,1):-1)

	    if ( .not. associated(get_val)) error stop 13
	    if ( any (lbound(get_val) .ne. (/ 0/))) error stop 24
	    if ( any (ubound(get_val) .ne. (/ 4/))) error stop 35

	    return

	entry ent(a) result (res)
	    if ( .not. associated(a%p)) error stop 20
	    res(1:) => a%p(1:20:2)
	    if ( .not. associated(res)) error stop 14
	    if ( any (lbound(res) .ne. (/1/))) error stop 24
	    if ( any (ubound(res) .ne. (/10/))) error stop 34
	end function
end module

    program main
	use m

	type(base(20,4)) :: b1

	allocate(b1%p(20), source = (/ (i,i=1,20) /) )

        b1%p(2:6) => b1%ent(b1)

	if ( .not. associated(b1%p)) error stop 12
	if ( any (lbound(b1%p) .ne. (/ 2/))) error stop 22
	if ( any (ubound(b1%p) .ne. (/ 6/))) error stop 32

	print *, b1%p
	print *, dot_product(b1%p, b1%get_val(b1))
    end program