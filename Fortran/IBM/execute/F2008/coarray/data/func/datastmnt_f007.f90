!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : datastmnt_f007.f
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
!*  DESCRIPTION                : Test simple initialization with DATA for whole array coarrays.
!*                            
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC

contains
	subroutine sub0()
		integer, save :: caf1(5)[1,2,*], caf2(2,2,2)[*]
		real, save :: caf3(2)[1,1,1,1:*], caf4(2)[3:*]
					
		data caf1/1,2,3,4,5/
		data caf2/1,1,2,3,5,8,13,21/
		data caf3,caf4/1.,2.,3.,4.5/
		
		if ( any(caf1 .ne. [1,2,3,4,5]) ) then
			print *, caf1
			error stop 21
		end if
		
		if ( any( reshape(caf2, (/8/)) .ne. [1,1,2,3,5,8,13,21]) ) then
			print *, reshape(caf2, (/8/))
			error stop 22
		end if
		
		if ( any(caf3 .ne. [1.0,2.0]) ) then
			print *, caf3
			error stop 23
		end if
		
		if ( any(caf4 .ne. [3.0,4.5]) ) then
			print *, caf4
			error stop 24
		end if
	end subroutine
	
end module


program main
	
	use modFDC
	call sub0()
	
end
