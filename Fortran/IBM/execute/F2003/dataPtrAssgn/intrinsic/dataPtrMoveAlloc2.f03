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
!* - data-ptr, TO and FROM are of type class(*), class(A) and class(B)
!*      where B is an extended type of A
!* - data-tar is array section of FROM, test if data-ptr associated with
!*        the array section of TO
!* - test type compatible and data-ptr's association status
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type A
    	integer id
    end type

    type, extends(A) :: B
    end type

end module

program main

    use m
    class(*), pointer ::  ptr(:)
    class(B), target, allocatable :: from(:)
    class(A), target, allocatable :: to(:)

    allocate(from(10), source = (/(B(i),i=1,10 )/))

    ptr(-2:) => from(1:10:2)

    if ( .not. associated(ptr, from(1:10:2))) error stop 5

    call move_alloc(from, to)

    if ( allocated(from) ) error stop 11
    if ( .not. allocated(to) ) error stop 13
    if ( .not. associated(ptr, to(1:10:2))) error stop 17
    if ( lbound(ptr,1) /= -2) error stop 18
    if ( ubound(ptr,1) /= 2) error stop 19

    select type(ptr)
	type is (B)
            if (any( ptr%id .ne. (/(i, i=1,10,2)/))) error stop 19

	class default
  	    stop 26
    end select

end program
