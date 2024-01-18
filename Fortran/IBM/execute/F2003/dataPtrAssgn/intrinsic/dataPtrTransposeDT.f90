!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrTransposeDT.f
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
!* - pointer as arg of transpose, derived-type 
!* - pointer is not poly, but target is poly with diff dynamic type
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    integer lb, ub
 
    type A
	integer x	
    end type

    type, extends(A) :: B
	real y	
    end type
	
    class(A), target, allocatable :: tar(:)

    contains
	function func(b1)
	    type(A), pointer :: b1(:,:)
	    type(A), allocatable :: func(:,:)

	    if ( .not. associated(b1)) stop 1
	    if ( any( lbound(b1) .ne. (/1,1 /) )) stop 3 
	    if ( any( ubound(b1) .ne. (/4,6 /) )) stop 5 

	    Do i = 1, ubound(b1,1) 
                 print *, b1(i,:)
	    end do

	    func = transpose(b1)

	end function
 
end module

    program main

	use m

	type(A), pointer :: p(:,:)
	type(A), allocatable :: f_tar(:,:)

	lb = 4 
	ub = 6

	allocate(tar(40), source = (/ ( B(i,real(i)),i=1,40) /) )

	p(1:lb, 1:ub) => tar 

	f_tar = func(p)

	if ( any( lbound(f_tar) .ne. (/1,1 /) )) stop 23
	if ( any( ubound(f_tar) .ne. (/6,4 /) )) stop 33

       	Do i = 1, ubound(f_tar,1) 
     	    print *, f_tar(i,:)
	end do


    end program
