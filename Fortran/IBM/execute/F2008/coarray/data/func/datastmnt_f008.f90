!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : datastmnt_f008.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : November 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test repeat count initialization with DATA for whole array coarrays.
!*                            
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	
	print *, func1()
	
end

integer function func1()

	integer, save :: caf1(10)[*], caf2(5,5)[1:2,-1:1,*]
	integer, save :: caf3(4)[5,0:*], caf4(2,2)[1,2,3,4,*]
	real, save :: caf5(1)[0:*], caf6(2,2,2,2)[*]
		
	data caf1/10*-1/
	data caf2/25*9/
	data caf3,caf4/8*99/
	data caf5,caf6/1*10., 16*6.5/
	
	if ( any(caf1 .ne. [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]) ) then
		print *, caf1
		error stop 21
	end if
	
	do i = 1, 5
		if ( any(caf2(i,:) .ne. [9,9,9,9,9]) ) then
			print *, caf2
			error stop 22
		end if
	end do
	
	if ( any(caf3 .ne. [99,99,99,99]) ) then
		print *, caf3
		error stop 23
	end if
	
	if ( any( reshape(caf4, (/4/)) .ne. [99,99,99,99] ) ) then
		print *, reshape(caf4, (/4/))
		error stop 24
	end if
	
	if ( any(caf5 .ne. (/10.0/)) ) then
		print *, caf5
		error stop 25
	end if

	if ( any( reshape(caf6, (/16/)) .ne. [6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5] ) ) then
		print *, reshape(caf6, (/16/))
		error stop 26
	end if
	
	func1 = 1
	
end function
