! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrMoveAlloc.f
! opt variations: -ql

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrMoveAlloc.f
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
!*  the types of data-ptr & FROM are extension of TO's type, FROM has same type
!*  as data-ptr
!*
!234567890123456789012345678901234567890123456789012345678901234567890
	module m
		type parent(k1)    ! (4)
	            integer, kind :: k1
	            integer(k1)      id
	 	end type
		type, extends(parent) :: child    ! (4)
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

		type(child(4)), target, allocatable ::  from(:)
		class(parent(4)), target, allocatable :: to(:)
		class(child(4)), pointer :: ptr(:,:)
		allocate(from(50), source= (/ (child(4)(i), i=1,50) /))

		ptr(-1*func('ab'):func('ab'), 6:func('cd')) => from(50:1:-1)

		call move_alloc(from, to)

		if ( allocated(from) ) stop 11
		if ( .not. allocated(to) ) stop 13

		select type (to)
		    type is (child(4))
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
