!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : datastmnt_f014.f
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
!*  DESCRIPTION                : Test repeat count initialization with DATA for array coarrays via implied-do loops.
!*                            
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	
	integer, save :: caf1(2,2,2)[*], caf2(3,3)[2,2,*]
	real, save :: caf3(8)[1,2,3,1:*]
	integer :: i, j, k
	
	data (((caf1(i,j,k), i=1,2), j=1,2), k=1,2) /2*8, 2*87, 2*66, 2*99/
	data ((caf2(i,j), i=1,3), j=1,3) /9*5/
	data (caf3(i), i=1,7,2) /4*1.0/, (caf3(i), i=2,8,2) /4*0.0/
	
	
	if ( any( reshape(caf1, (/8/)) .ne. [8, 8, 87, 87, 66, 66, 99, 99]) ) then
		print *, reshape(caf1, (/8/))
		error stop 21
	end if
	
	if ( any(reshape(caf2, (/9/)) .ne. [5, 5, 5, 5, 5, 5, 5, 5, 5]) ) then
		print *, reshape(caf2, (/9/))
		error stop 22
	end if

	if ( any(caf3 .ne. [1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0]) ) then
		print *, caf3
		error stop 23
	end if
	
end
