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
!*  If TO has the TARGET attribute, any pointer associated with FROM on entry to
!*  MOVE_ALLOC becomes correspondingly associated with TO.
!*  Pointer is a component of type class(Child), Target(TO) is of class(Parent)
!*  FROM is of type(child)
!*
!234567890123456789012345678901234567890123456789012345678901234567890
	module m
		type parent
	            integer id
	 	end type
		type, extends(parent) :: child
		end type

		contains
	            function func(ch)
			character(len=2) :: ch
			integer, pointer :: func

			if ( ch == 'ab') allocate(func,source=3)
			if ( ch == 'cd') allocate(func,source=9)
		    end function
	end module

	program main
		use m

		type(child), target, allocatable ::  from(:)
		class(parent), target, allocatable :: to(:)
		class(child), pointer :: ptr(:,:)
		allocate(from(50), source= (/ (child(i), i=1,50) /))

		ptr(-1*func('ab'):func('ab'), 6:func('cd')) => from(50:1:-1)

		call move_alloc(from, to)

		if ( allocated(from) ) stop 11
		if ( .not. allocated(to) ) stop 13

		select type (to)
		    type is (child)
			if ( .not. associated(ptr) ) stop 15
			print *, lbound(ptr)
			if ( any(lbound(ptr) .ne. (/ -3, 6/))) stop 17
			if ( any(ubound(ptr) .ne. (/3,9 /))) stop 19
			if ( any(ptr%id .ne. reshape((/(i,i=50,23,-1)/), &
					(/7,4/)))) stop 23
		    class default
			stop 21
		end select

	End program
