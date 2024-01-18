!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_a011.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : March 2011
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  DESCRIPTION
!*
!*  Assign host associated data to Derived Type coarray components
!*  (scalars and arrays of different kinds) and vice versa
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program main

	type train
		character(3) :: driver
		integer*4 :: occupants_per_car(12)
	end type
	type(train), save, codimension[0:1,0:*] :: younge
	type(train), save, codimension[1:*] :: ttc(3)

	character(3), parameter :: D = "ITA"
    integer(4), parameter :: min = 1_4,  max = 500_4, mid = 47_4

    call sub

contains

	subroutine sub

		integer(4) :: arr(12)
		character(3) :: dsub, darr(3)
		
		dsub = D
		arr(1:4) = min
		arr(5:8) = mid
		arr(9:12) = max
		
		darr = [dsub, "GER", "NED"]
		
		! start with setting the coarray
		younge%driver = dsub
		younge%occupants_per_car = arr 
		
		ttc(:)%driver = darr
		ttc(1)%occupants_per_car = arr
		ttc(2)%occupants_per_car(1:4) = mid
		ttc(2)%occupants_per_car(5:12) = [12,13,14,15,16,17,18,19]
		ttc(3)%occupants_per_car(1:4) = [32,5,19,26]
		ttc(3)%occupants_per_car(5:12) = arr(5:12) 
		
		if (younge%driver .ne. D) then
			print *, "actual", younge%driver
			print *, "expected", D 
			error stop 15
		end if	
		
		if ( any(younge%occupants_per_car .ne. arr) ) then
			print *, "actual", younge%occupants_per_car
			print *, "expected", arr 
			error stop 16
		end if
		
		if ( any(ttc%driver .ne. darr) ) then
			print *, "actual", ttc%driver
			print *, "expected", darr 
			error stop 17
		end if
		
		if ( any(ttc(1)%occupants_per_car .ne. arr) ) then
			print *, "actual", ttc(1)%occupants_per_car
			print *, "expected", arr 
			error stop 18
		end if
		
		if ( any(ttc(2)%occupants_per_car .ne. [47,47,47,47,12,13,14,15,16,17,18,19]) ) then
			print *, "actual", ttc(2)%occupants_per_car
			error stop 19
		end if
		
		if ( any(ttc(3)%occupants_per_car .ne. [32,5,19,26,47,47,47,47,500,500,500,500]) ) then
			print *, "actual", ttc(3)%occupants_per_car
			error stop 20
		end if
		
		
		! assignment from coarrays
		dsub = ""
		dsub = younge%driver
		arr = 0
		arr(1:8) = younge%occupants_per_car(5:12)
		arr(9:12) = younge%occupants_per_car(1:4)
		darr = ""
		darr = ttc(:)%driver
		
		if (dsub /= D) then
			print *, "actual", dsub
			print *, "expected", D 
			error stop 21
		end if
		
		if ( any(darr /= ttc(:)%driver) ) then
			print *, "actual", dsub
			print *, "expected", ttc(:)%driver
			error stop 22
		end if
		
		if ( any(arr /= [47,47,47,47,500,500,500,500,1,1,1,1]) ) then
			print *, "actual", dsub
			print *, "expected", younge%occupants_per_car
			error stop 23
		end if
		
  	end subroutine sub

end
