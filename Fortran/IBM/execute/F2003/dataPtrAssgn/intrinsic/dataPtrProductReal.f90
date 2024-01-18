!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrProductReal.f 
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
!* - data-targets are function result & entry result
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    contains
    function func(a, lb)
	integer, optional :: lb
	real, allocatable, target :: a(:)
	real, pointer :: func(:)
	real, pointer :: ent(:)

	if ( .not. allocated(a)) stop 11

	if ( present(lb) ) then
            func(lb:) => a(::2)
	else
	    func => a(::2)
	endif

	if ( .not. associated(func, a(::2))) stop 13
	if ( lbound(func, 1) /= lb ) stop 15
	if ( ubound(func, 1) /= 12 ) stop 17

	return

    entry ent(a)

	if ( .not. allocated(a)) stop 23

	ent(11:size(a)+5) => a(2::2)

	if ( .not. associated(ent)) stop 25 
	if ( lbound(ent, 1) /= 11 ) stop 27 
	if ( ubound(ent, 1) /= 15 ) stop 29 
    end function 

end module 

program main

    use m

    type base
	real, pointer :: p(:)
    end type
 
    type(base) :: b

    real*4, allocatable :: elm(:)

    allocate(elm(10), source=(/ (i*2.0, i=1,10 ) /))	

    b%p => func(elm, 8) 

    write(*, '(5f10.3)') b%p
    write(*, '(5f10.3)') product(b%p)

    deallocate(elm)

    allocate(elm(10), source=(/ (real(i-10,4), i=11,20) /))	
    
    b%p => ent(elm)
    write(*, '(5f10.3)') b%p
    write(*, '(5f10.3)') product(b%p)

end program
