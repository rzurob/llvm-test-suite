!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrMoveAlloc1.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* -If TO has the TARGET attribute, any pointer associated with FROM on entry to
!* MOVE_ALLOC becomes correspondingly associated with TO.
!* - the type of FROM is extended from data-ptr's.TO is of class(*)
!* -TO/FROM/Pointer are rank-4 arrayes; for bounds-spec list, use kind as lb
!*
!234567890123456789012345678901234567890123456789012345678901234567890
	program main

		type parent
		    integer :: id
	 	end type

		type, extends(parent) :: child
	            character(:), allocatable :: ch
		end type

		type(child), target, allocatable ::  from(:,:,:,:)

		class(*), target, allocatable :: to(:,:,:,:)

		class(parent), pointer :: ptr(:,:,:,:)

		allocate(from(2,3,2,2), source= reshape( (/(child(i,achar(i)), &
                                i=67,90)/), (/2,3,2,2/) ) )

		! bounds-spec list
		! shape of ptr (2,2,2,2); shape of from(2,2,2,2)
		ptr(kind('c'):,kind(1):,kind(1_8):,kind(0.0_16):) => from(:,2:,:,:)

		call move_alloc(from, to)

		if ( allocated(from) ) stop 11
		if ( .not. allocated(to) ) stop 13

		select type(to)
		    type is (child)
			if ( .not. associated(ptr)) stop 15
		    class default
			stop 17
		end select

		if ( any(lbound(ptr) .ne. (/1,4,8,16/))) stop 17
		If ( any(ubound(ptr) .ne. (/2,5,9,17 /))) stop 21

		select type(ptr)
	  	    type is (child)
			print *, ptr%id
			do i = 16,17
			   do j = 8,9
		              do k=4,5
			         do m = 1,2
		                    print *, ptr(m,k,j,i)%ch
			         enddo
			      enddo
	                   enddo
		        enddo
		    class default
			stop 23
		end select

		! readjust the lower-bound of ptr to be 1
		ptr(1:, 1:, 1:, 1:) => ptr

		! elements of TO is redefined
		select type(ptr)
		    type is (child)
		        ptr(:, 2, 2, 2) = (/ child(101, 'hello'),  &
				child(103,'IBM')/)
		    class default
		        stop 25
	        end select

		select type(to)
	  	    type is (child)
			print *, to%id
			do i = 1,2
			   do j = 1,2
		              do k= 1,3
			         do m = 1,2
		                    print *, to(m,k,j,i)%ch
			         enddo
			      enddo
	                   enddo
		        enddo
		    class default
			stop 23
		end select

	End program
