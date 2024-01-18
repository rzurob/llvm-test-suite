! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrNECmplx.f
! opt variations: -qnol -qnodeferredlp

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
!* - lb/ub of data-ptr is pure type bound proc
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type base(n1,k1,k2)    ! (20,4,16)
	integer, kind        :: k1,k2
	integer, len         :: n1
	integer(k1)          :: lb, size
	complex(k2), pointer :: ptr(:)
	contains
	    procedure, pass :: get_lb
	    procedure, pass :: allocation
    end type

    contains
 	integer pure function get_lb(b)
	    class(base(*,4,16)), intent(in) :: b
	    get_lb = b%lb
 	end function
 	subroutine allocation(b)
	    class(base(*,4,16)), intent(inout) :: b

	    allocate(b%ptr(b%size), source = (/(cmplx(i,i,16),i=1,b%size )/))
 	end subroutine

end module

program main

    use m
    class(base(:,4,16)), allocatable :: a

    allocate(a, source = base(20,4,16)(2, 10, null()) )

    call a%allocation()

    a%ptr(a%get_lb():a%get_lb()+a%size/2-1) => a%ptr(::2)

    if ( .not. associated(a%ptr)) stop 1
    if ( lbound(a%ptr,1) /= 2) stop 2
    if ( ubound(a%ptr,1) /= 6) stop 3

    write (*, '("(",f20.15,", ", f20.15, ")")') a%ptr
    if ( anY ( a%ptr /= cmplx(-2,3, 16) .neqv. .true.)) stop 5
end program
