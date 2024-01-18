! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrCshiftDT.f
! opt variations: -ql

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrCshiftDT.f 
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
!* - data-pointer of derived type, as arg of cshift 
!* - the left part-name of data-target is an array of derived type
!* - the right part-name of data-target is scalar component
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id
    end type

    type, extends(base) :: A(k2)    ! (4,8)
	integer, kind            :: k2
	logical(k2), allocatable :: ll 
    end type
    
end module

    program main

        use m

        class(base(4)), pointer :: p1(:,:,:)
        type(A(4,8)), target :: tar1(4,4,4)
        type(base(4)), allocatable :: res(:,:,:)

        tar1 = reshape( (/ ( A(4,8)(i,logical(mod(i,2) == 0,8)) ,i=1,64 ) /) , (/4,4,4/)) 

        p1(2:,1:,3:) => tar1(1:3,2:3,2:4)%base

        if ( .not. associated(p1)) stop 11
        if ( any (lbound(p1) .ne. (/2,1,3/) )) stop 13
        if ( any (ubound(p1) .ne. (/4,2,5/) )) stop 17

	select type (p1)
	    type is (base(4))
		print *, p1%id
	    class default
		stop 19
	end select

        res = cshift(p1, reshape((/-1,1,1,0,-1,0/)  ,(/3,2 /) ) , dim = 3)
	print *, shape(res)
	print *, res%id 

    end program
