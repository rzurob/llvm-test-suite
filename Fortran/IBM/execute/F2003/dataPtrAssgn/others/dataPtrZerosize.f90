!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrZerosize.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - target is zero-size array, use bounds-spec-lst for pointer
!* - bounds-remapping-list defines zero-size pointer
!* - check lbound and ubound
!*
!234567890123456789012345678901234567890123456789012345678901234567890


   program main

	double precision,  target, allocatable :: tar(:,:,:)
	double precision, target :: tar2(100)
	double precision, pointer :: ptr(:,:,:)

	allocate(tar(0,1:0, 0:0))

	if ( .not. allocated(tar)) stop 3

        ! target is zero-size for some sections, bounds-spec-list
	ptr(3:,0:,7:) => tar

	if ( associated(ptr, tar) ) stop 5
	if (any(lbound(ptr) .ne. (/1,1,7/))) stop 7
	if (any(ubound(ptr) .ne. (/0,0,7/))) stop 9

	! pointer is zero-size, bounds-remapping-list
	i = -2
	call sub (i)
	if (any(lbound(ptr) .ne. (/1,1,1/))) stop 27
	if (any(ubound(ptr) .ne. (/0,0,0/))) stop 29

 	contains
	    function set_bd(i)
		integer i, set_bd
		allocatable set_bd
		allocate(set_bd, source = i)
	    end function

	    subroutine sub(i)
		integer i
	        ptr(i:i-1, 1:set_bd(0), int(3.2):i) => tar2
	    end subroutine
        End program
