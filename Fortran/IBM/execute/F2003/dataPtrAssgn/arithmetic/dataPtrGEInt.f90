!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrGEInt.f 
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
!* - data-target is type bound procedure pointer
!* - the part of data-ptr is redfined by itself
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type base
    end type
end module

module n
    use m

    type, extends(base) :: A
	integer*8, pointer :: p(:,:,:)
    end type 

    type, extends(base) :: B
	procedure(func), nopass, pointer :: pp => null()
    end type

    interface
	function func(arg)
	    integer*8, pointer :: func(:)
	    integer :: arg(:)
	end function 
    end interface
end module

program main

    use n

    type(B) :: b1
    type(A), allocatable :: a1

    b1%pp => func

    a1 = A(null())

    a1%p(1:2,3:4,5:7) => b1%pp( [-1,0,1,2,3,4,5,6,7,8,9,10])  

    if ( .not. associated(a1%p)) stop 1 
    if ( any (lbound(a1%p) .ne. (/1,3,5/))) stop 2
    if ( any (ubound(a1%p) .ne. (/2,4,7/))) stop 3 

    if ( any(a1%p(2,3,:) .ge. (/0,4,8 /)) ) then
    	a1%p(2,3,:) = a1%p(2,3, 7:5:-1) 
    endif

    print *, a1%p

end program

	function func(arg)
	    integer*8, pointer :: func(:)
	    integer :: arg(:)

	    allocate(func(size(arg)), source = int(arg,8)) 
	end function 
