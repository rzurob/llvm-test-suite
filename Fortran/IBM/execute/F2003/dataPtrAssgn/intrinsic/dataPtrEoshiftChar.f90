!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrEoshiftChar.f 
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
!* - date-pointer of class(*), array pointer
!* - data-target is type bound proc with pass attribute 
!* - the proc returns array pointer of character(:) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m


	type base
	    character(2), allocatable :: p
	    contains
		procedure :: fun => func
	end type

	contains

	   function func(a, ch)
		character(:), pointer :: func(:)
		class(base), intent(in) :: a
		character(:), allocatable :: ch(:)

		allocate(func(size(ch)), source= ch) 
	    end function

end module

    program main

        use m

	class(*), pointer :: p(:)
	type(base), allocatable :: b2
	character(:), allocatable :: ch(:)

	ch = (/ (repeat(achar(i+64),2), i=1,26) /)

	allocate(b2)

	allocate(integer :: p(0))

	p(size(p):) => b2%fun(ch)

	if ( .not. associated(p)) stop 5 
	if ( lbound(p,1) /= 0) stop 7
	if ( ubound(p,1) /= 25) stop 9

	select type (p)
	    type is (character(*))
		print *, p
		print *, eoshift(p, 2)
	    class default
		stop 50
	end select

    end program
