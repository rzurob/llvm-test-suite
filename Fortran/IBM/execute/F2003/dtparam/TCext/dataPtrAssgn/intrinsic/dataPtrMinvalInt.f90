! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrMinvalInt.f
! opt variations: -qnok -qnol -qnodeferredlp

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrMinvalInt.f 
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
!* - data-pointer/target is component of DT, of type class(*)
!* - dynamic type is integer*8, redefined by where statement 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    module m
	type base(k1,n1)    ! (4,20)
	    integer, kind :: k1
	    integer, len  :: n1
	    class(*), pointer :: p(:, :)
	end type

	type, extends(base) :: child    ! (4,20)
	    class(*), pointer :: t(:)
	end type

    end module

    program main

	use m	

	class(base(4,:)), allocatable :: b1
 
	allocate(child(4,20)::b1)

	select type (b1)
	    type is (child(4,*))
	        allocate(b1%t(36), source = (/ ( int(i,8), i=1,36) /) )
 		b1%p(2:6,0:5) => b1%t(36:1:-1)

	        if ( .not. associated(b1%p)) stop 12
	        if ( any ( lbound(b1%p) .ne. (/2,0/) )) stop 22
	        if ( any ( ubound(b1%p) .ne. (/6,5/) )) stop 32
	    class default
	        stop 22
	end select	

	select type (x => b1%P)
	    type is (integer*8)
		where ( mod(x,2_8) == 1) 
			x = -x 
		end where
	        print *,x 
		print *, minval(x, dim=2) , minval(x, dim=1)
	    class default
		stop 42 
	end select

    end program
