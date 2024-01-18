!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : datastmnt_f017.f
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
!*  DESCRIPTION                : Test initialization with DATA statements of derived type coarray scalars (whole).
!*                            
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC

contains
	subroutine sub0(li1, li2)
		type point
			sequence
			integer :: x, y
		end type
		
		type line
			sequence
			type (point) :: P1, P2
		end type
		type (line) :: li1[*], li2[1:2,1:3,1:*]
	
		
		if ((li1%P1%x /= 1) .or. (li1%P1%y /= 2)) then
			print *, li1%P1%x, li1%P1%y
			error stop 22
		end if
		
		if ((li1%P2%x /= 3) .or. (li1%P2%y /= 4)) then
			print *, li1%P2%x, li1%P2%y
			error stop 23
		end if
		
		if ((li2%P1%x /= 0) .or. (li2%P1%y /= 0)) then
			print *, li2%P1%x, li2%P1%y
			error stop 24
		end if
		
		if ((li2%P2%x /= 9) .or. (li2%P2%y /= -1)) then
			print *, li2%P2%x, li2%P2%y
			error stop 25
		end if
	end subroutine
end module


program main

	use modFDC
	type point
		sequence
		integer :: x, y
	end type
	
	type line
		sequence
		type (point) :: P1, P2
	end type
	type (line), save :: L1[*], L2[1:2,1:3,1:*]
	
	
	data L1 /line(point(1, 2), point(3, 4))/
	data L2 /line(point(0, 0), point(9, -1))/
	
	call sub0(L1, L2)
	
end
