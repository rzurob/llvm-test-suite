!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_a010.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : March 2011
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  DESCRIPTION
!*
!*  Assign use associated data to Derived Type coarray components
!*  (scalars and arrays of different kinds) and vice versa
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module modSTP
	integer, allocatable :: x
	real, pointer :: ptr
	integer, dimension(2,2,2) :: arr
	
	type globalDT
		complex :: c
	end type
	type(globalDT), save, codimension[*] :: coarray1, coarray2(2)
end module


program main
	
	use modSTP
	
	type DT
		sequence
		integer :: int 
		real :: re
	end type
	type(DT), save :: caf[3,4,*], cafar(2,2,2)
	codimension cafar[5,*]

    real, target :: targ
    
    ! assign to coarray
    arr = reshape([1,2,3,4,5,6,7,8], [2,2,2])
    allocate(x)
    x = 3
    targ = 1.5
    ptr => targ
    
    caf%int = x
    caf%re = ptr
    cafar%int = arr
    cafar(:,:,:)%re = ptr
    
    if ( (caf%int /= x) .or. (caf%re /= ptr) ) then
    	print *, caf
    	error stop 14
    end if
    
    if ( any(cafar%int .ne. arr) ) then
    	print *, "actual", cafar%int
    	print *, "expected", arr
    	error stop 15
    end if
    
    if ( any(reshape(cafar%re, [8]) .ne. [(ptr, i = 1,8)]) ) then
    	print *, "actual", cafar%re
    	print *, "expected", [(ptr, i = 1,8)]
    	error stop 16
    end if
    
    
    ! assign from coarray
    caf%int = 0
    caf%re = -0.5
    cafar(:,:,:)%int = -1
    
    x = caf%int
    ptr = caf%re
    arr = cafar(:,:,:)%int
    
    if ( (x /= caf%int) .or. (ptr /= caf%re) ) then
    	print *, "actual", x, ptr
    	print *, "expected", caf%int, caf%re
    	error stop 17
    end if
    
    if ( any(arr .ne. cafar%int) ) then
    	print *, "actual", arr
    	print *, "expected", cafar%int
    	error stop 18
    end if
    

	call sub0()
end


subroutine sub0()
	use modSTP
	complex, parameter :: plex = (-1.0,2.5)
	complex :: cool, ciss(2)
	
	coarray1%c = plex
	coarray2(:)%c = plex * (-10)
	
	if (coarray1%c .ne. plex) then
		print *, "actual", coarray1%c
		print *, "expected", plex
		error stop 30
	end if
	
	if ( any(coarray2%c .ne. [(plex * (-10), i = 1,2)]) ) then
		print *, "actual", coarray2%c
		print *, "expected", plex * (-10)
		error stop 31
	end if
	
	
	cool = coarray1%c - 1
	ciss = coarray2%c / (-10)
	
	if (cool .ne. (plex - 1)) then
		print *, "actual", cool
		print *, "expected", plex - 1
		error stop 32
	end if
	
	if ( any(ciss .ne. [(plex, i = 1,2)]) ) then
		print *, "actual", ciss
		print *, "expected", plex
		error stop 33
	end if
end subroutine

