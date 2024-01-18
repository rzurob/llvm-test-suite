!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : datastmnt_f010.f
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
!*  DESCRIPTION                : Test repeat count initialization with DATA for array coarray elements.
!*                            
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC
	integer, save :: caf1(5)[0:*], caf2(2,2,2)[1,2,3,4,5,6,7,*]
	real, save :: caf3(2)[-1:-1,1,-1:2,1:*], caf4(2)[*]
	
	data caf1(1), caf1(2), caf1(3), caf1(4), caf1(5) /2*-1, 3*1/
	data caf2(1,1,1), caf2(1,1,2), caf2(1,2,1), caf2(1,2,2), &
			caf2(2,1,1), caf2(2,1,2), caf2(2,2,1), caf2(2,2,2) /6*4, 5, 6/
	data caf3(1), caf3(2) ,caf4(1), caf4(2)/4*0.0/
end module


program main

	use modFDC
	
	if ( any(caf1 .ne. [-1,-1,1,1,1]) ) then
		print *, caf1
		error stop 21
	end if
	
	if ( any( reshape(caf2, (/8/)) .ne. [4,4,4,5,4,4,4,6]) ) then
		print *, reshape(caf2, (/8/))
		error stop 22
	end if
	
	if ( any(caf3 .ne. [0.0,0.0]) ) then
		print *, caf3
		error stop 23
	end if
	
	if ( any(caf4 .ne. [0.0,0.0]) ) then
		print *, caf4
		error stop 24
	end if
	
end
