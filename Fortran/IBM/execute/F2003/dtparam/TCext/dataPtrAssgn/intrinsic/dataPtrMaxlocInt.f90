! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrMaxlocInt.f
! opt variations: -qnok -qnol

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrMaxlocInt.f 
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
!* - data-pointer associates with the selector of associate construct 
!* - data-pointer is redefined by redefining the selector
!*
!234567890123456789012345678901234567890123456789012345678901234567890


    module m

	integer*2, target, allocatable :: t1(:)

	type base(k1,n1)    ! (4,20)
	    integer, kind :: k1
	    integer, len  :: n1
	    class(*), pointer :: p(:, :)
	end type

    end module

    program main

	use m	

	type(base(4,20)) :: b1

	allocate(t1(8), source = (/ (int(i,2), i=1,8 ) /) ) 

 	b1%p(1:2,1:4) => t1

	if ( .not. associated(b1%p)) stop 12
	if ( any ( lbound(b1%p) .ne. (/1,1/) )) stop 22
	if ( any ( ubound(b1%p) .ne. (/2,4/) )) stop 32

	select type (x =>b1%p )
	    type is (integer*2)
		associate( arg => x)
		    arg = arg(:,4:1:-1) 
		    print *, arg 
		    print *, maxloc(arg)	
		end associate
	    class default
	        stop 22
	end select	

    end program
