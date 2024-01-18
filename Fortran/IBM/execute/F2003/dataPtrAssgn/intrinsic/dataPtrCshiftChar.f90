!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrCshiftChar.f 
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
!* - date-pointer a component of DT of type character(:)
!* - data-target is type bound proc of the same object as data-pointer
!* - the proc returns array pointer of type char
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

	type base
	    character(:), pointer :: p(:)
	    contains
		procedure, nopass :: get_char 
	end type

	contains

	    function get_char(a)
		type(base), intent(in) :: a
		character(2), pointer :: get_char(:)

		allocate(get_char(size(a%p)), source= a%p) 
	    end function

end module

    program main

        use m

	type(base) :: b1, b2 

	allocate(b2%p(26), source = (/ (repeat(achar(i+64),2), i= 1,26)  /) )

	b1%p(2:size(b2%p)) => b1%get_char(b2)

	if ( .not. associated(b1%p)) stop 5 
	if ( lbound(b1%p,1) /= 2) stop 7
	if ( ubound(b1%p,1) /= 26) stop 9

	print *, b1%p
	print *, cshift(b1%p, 24)

    end program
