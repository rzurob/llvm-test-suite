!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :dataPtrProductInt.f
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
!* - data-pointer is component of DT, rank 2
!* - lbounds of data-pointer are function call/entry call to 
!*          type-bound proc w pass attr
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base
	integer, pointer :: p(:,:)
	contains	
	    procedure, pass :: ent
	    procedure, pass :: get_val
    end type

    contains
	function get_val(a)
	    class(base), intent(in) :: a
	    integer :: res
	    integer :: get_val
	    get_val = dot_product(lbound(a%p), ubound(a%p) )
	    return
 
            entry ent(a) result (res)
	    res = product( ubound(a%p) )
	end function
end module

    program main
	use m

	type(base) :: b1

	allocate(b1%p(4,5), source = reshape( (/ (i,i=1,20) /), &
		(/4,5/) ))

        b1%p(b1%get_val():,b1%ent():) => b1%p(::2,::2)

	if ( .not. associated(b1%p)) stop 12
	if ( any (lbound(b1%p) .ne. (/ 9,20/))) stop 22
	if ( any (ubound(b1%p) .ne. (/ 10,22/))) stop 32

	print *, b1%p
	print *, product(b1%p,1), product(b1%p,2), product(b1%p)

    end program
