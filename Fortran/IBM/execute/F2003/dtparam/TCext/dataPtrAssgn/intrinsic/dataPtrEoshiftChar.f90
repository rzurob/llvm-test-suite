! GB DTP extension using:
! ftcx_dtp -qk -qdeferredlp /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrEoshiftChar.f
! opt variations: -qck -qnok -qnodeferredlp

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
!* - date-pointer of class(*), array pointer
!* - data-target is type bound proc with pass attribute
!* - the proc returns array pointer of character(:)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m


	type base(k1,n1)    ! (4,2)
	    integer, kind              :: k1
	    integer, len               :: n1
	    character(n1), allocatable :: p
	    contains
		procedure :: fun => func
	end type

	contains

	   function func(a, ch)
		character(:), pointer :: func(:)
		class(base(4,*)), intent(in) :: a
		character(:), allocatable :: ch(:)

		allocate(func(size(ch)), source= ch)
	    end function

end module

    program main

        use m

	class(*), pointer :: p(:)
	type(base(4,:)), allocatable :: b2
	character(:), allocatable :: ch(:)

	ch = (/ (repeat(achar(i+64),2), i=1,26) /)

	allocate(base(4,2) :: b2)

	allocate(integer :: p(0))

	p(size(p):) => b2%fun(ch)

	if ( .not. associated(p)) error stop 5
	if ( lbound(p,1) /= 0) error stop 7
	if ( ubound(p,1) /= 25) error stop 9

	select type (p)
	    type is (character(*))
		print *, p
		print *, eoshift(p, 2)
	    class default
		stop 50
	end select

    end program
