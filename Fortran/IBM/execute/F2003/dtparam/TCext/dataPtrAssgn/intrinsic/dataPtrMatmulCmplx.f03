! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrMatmulCmplx.f
! opt variations: -qnok -ql

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
!* - data-pointer used as arg of matmul
!* - type complx, bounds-remapping-list
!* - pointer is dummy arg of external func, lb/ub of pointer is dummy arg
!* - pointer's declared type is class(*)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type A(k1)    ! (4)
        integer, kind :: k1
	class(*), pointer :: p(:,:)
    end type

    interface foo
	function func(a,b,p)
	    integer a, b
	    complex, pointer :: func(:,:)
	    class(*), pointer :: p(:,:)
	end function
    end interface
end module

    program main

	use m

	type(A(4)) :: aT
	complex, allocatable :: res(:,:)

	res = foo(4,5,aT%p)

	if ( .not. associated(aT%p)) error stop 11
	if ( any( lbound(aT%p) .ne. (/0,1 /) )) error stop 13
	if ( any( ubound(aT%p) .ne. (/0,4 /) )) error stop 13

	select type ( x => aT%p)
	     type is (complex)
		write (*, '(2f12.6)') matmul(x,res)
	    class default
	        stop 9
	end select

    end program

	function func(a,b,p)
	    integer a, b
	    complex , pointer :: func(:,:)
	    class(*), pointer :: p(:,:)

            allocate(func(a,b), source =reshape((/ &
                       ((cmplx(i,j),i=1,a),j=1,b) /) , (/a,b/)))

	    p(0:0, b-a:a) => func(2, 2:b)

	end function