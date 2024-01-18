! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/dataPtrAssgn/others/dataPtrArgbound.f
! opt variations: -qnok -qnol

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrArgbound.f 
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
!* - lb/ub of pointer are dummy arguments of external procedure
!* - data-pointer is a DT component of type class(*), dynamic type is integer*2 
!*
!234567890123456789012345678901234567890123456789012345678901234567890
 
   module m
	integer, save :: count = 0

	type base(k1,n1)    ! (4,20)
	    integer, kind :: k1
	    integer, len  :: n1
	    class(*), pointer :: p(:,:) 
	end type

	type(base(4,20)) :: b1

	interface 
	    subroutine sub(lb, ub)
		integer lb(2), ub(2)
	    end subroutine	
	end interface

   end module

   program main
        use m

	allocate(integer*2 :: b1%P(20,20))
	if ( .not. associated(b1%p)) stop 1
  
	select type ( x => b1%p)
	    type is (integer*2)
		do j = 1, 20
		    !IBM* loopid(loop)
		    do i = 1, 20
			count = count + 1
			x(i,j) = count
		    enddo
		enddo
	    class default
		stop 3
	end select
	 
	call sub( (/7, -1/), (/11, 1/) )

	if ( .not. associated(b1%p)) stop 11 
	if ( any(lbound(b1%p) .ne. (/7,-1/))) stop 5
	if ( any(ubound(b1%p) .ne. (/11,1/))) stop 7

	select type ( x => b1%p)
	    type is (integer*2)
		print *, x
	    class default
		stop 9 
	end select

  End program

  subroutine sub(lb, ub)
	use m, only : base, b1 
	integer lb(2), ub(2)

	b1%p(lb(1):ub(1), lb(2):ub(2)) => b1%p(:, 20) 
  end subroutine	
