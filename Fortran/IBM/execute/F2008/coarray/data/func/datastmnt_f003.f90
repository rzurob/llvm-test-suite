!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : November 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test simple initialization with DATA for complex coarray scalars.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC

contains
	real function f1(c1, c2)
		complex(4), save :: c1[0:2,0:*]
		complex(8), save :: c2[1,2,4,*]

		if (c1 /= (1.0,-1.0)) then
			print *, c1
			error stop 21
		end if

		if (c2 /= (-3.5,5.0)) then
			print *, c2
			error stop 22
		end if

		f1 = 1.0
	end function
end module


program main

	use modFDC
	complex(4), save :: caf1[*]
	complex(8), save :: caf2[1,2,3,4:5,6:7,8:*]
	integer :: res

	data caf1,caf2/(1.0,-1.0), (-3.5,5.0)/

	res = f1(caf1, caf2)

end
