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
!* - if ptr is not poly, target is poly with different dynamic type, the
!*  assignment target is the ancestor component of target
!* - ptr is of type(base); tar's dynamic type is of class(child), its declared
!*  type is of class(base);
!* - ptr is of class(*); tar is of class(child)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

   program main

	type base
	    integer :: id
	end type

 	type, extends(base) :: child
	    character(len=3) :: ch
	end type

	type(base), pointer :: p(:)
	class(base), target,  allocatable :: t(:)
	class(*), pointer :: q(:)

	allocate(t(200), source = (/ ( child(i,'IBM'),i=1,200 ) /) )

	p(1:t(100)%id) => t(200:1:-1)

	if ( .not. associated(p)) error stop 1
	if ( lbound(p,1) /= 1) error stop 3
	if ( ubound(p,1) /= 100) error stop 5

	q(20:) => p

	if ( .not. associated(q,p)) error stop 11
	if ( lbound(q,1) /= 20) error stop 13
	if ( ubound(q,1) /= 119) error stop 15

	select type (q)
	    type is (child)
		stop 21
	    type is (base)
		print *, (/ (q(i)%id, i=lbound(q,1),ubound(q,1) ) /)
	    class default
	 	stop 23
	end select

   end program
