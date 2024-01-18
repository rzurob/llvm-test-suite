! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/others/dataPtrTarProcComp.f
! opt variations: -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrTarProcComp.f 
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
!* - data-target is procedure function component which returns a data pointer 
!* - lb/ub is a module variable 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    integer :: lbd, ubd

    type base(n1,k1)    ! (20,4)
	integer, kind :: k1
	integer, len  :: n1
	integer(k1)   :: x
 	procedure(basefunc), nopass, pointer :: pp  
    end type

    contains

	function basefunc(i, lb, ub)
	    integer i
	    integer, optional :: lb, ub
            type(base(:,4)), pointer :: basefunc(:), tmp(:)

	    lbd = lb
	    ubd = ub
	    allocate(tmp(i), source=(/ (base(20,4)(j,null()),j=1,i) /) )
	    basefunc(lb:) => tmp(ub:1:-1)

	end function 
end module
 
    program main
	use m

	type(base(20,4)) :: b1
	procedure(basefunc), pointer :: pp	
	class(base(:,4)), pointer :: p(:)

 	b1%pp => basefunc 

	p(lbd:ubd) => b1%pp(100,20,50)

	if ( .not. associated(p) ) stop 13
	if ( lbound(p,1) /= 20 ) stop 15
	if ( ubound(p,1) /= 50 ) stop 17 
	print *, p%x

    end program
