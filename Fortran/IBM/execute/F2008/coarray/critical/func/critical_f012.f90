!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : January 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test critical construct in an internal procedure
!*                               using an explicit shape dummy coarray.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	complex(4), save :: c1(4)[-1:*]
	complex(8), save :: c2(2,2,2)[2,1,*]

	c1 = (0.0, 0.0)
	c1(1) = (5.0, 5.0)
	c2 = (0.0, 0.0)

	if (num_images() > 1) then
		if (oneD(c1) /= 1) then
			print *, "TEST1: failed"
			error stop 21
		end if

		if (threeD(c2) /= 1) then
			print *, "TEST2: failed"
			error stop 22
		end if
	end if

contains

	integer function oneD(coarray)
		complex(4) :: coarray(4)[-1:*], tmp, expected(4)

		expected = (1.0, 0.0)
		expected(1) = (4 * num_images()) + 1
		expected(2) = (4 * num_images()) + 2
		expected(3) = (4 * num_images()) + 3
		expected(4) = (4 * num_images()) + 4
		tmp = 0.0
		coarray = 0.0
		sync all

		critical
			do i = 1, 4
				tmp = coarray(i)[-1] + 1
				coarray(i)[-1] = tmp
			end do
		end critical

		!Post Condition
		sync all
		if ( any(coarray[-1] .ne. expected) ) then
			print *, coarray[-1]
			print *, expected
			error stop 14
		end if
		oneD = 1
	end function

	integer function threeD(coarray)
		complex(8) :: coarray(2,2,2)[*]
		complex(8) :: expected1(8), expected2(8), tmp

		expected1 = (0.0, 0.0)
		expected2 = (0.0, 0.0)
		expected1(1) = (num_images(), 0.0)
		expected2(8) = (-num_images(), 0.0)

		critical
			tmp = coarray(1,1,1)[1] + 1
			coarray(1,1,1)[1] = tmp
			tmp = coarray(2,2,2)[2] - 1
			coarray(2,2,2)[2] = tmp
		end critical

		!Post Condition
		sync all

		if ( any(reshape(coarray[1], [8]) .ne. expected1) ) then
			print *, coarray[1]
			print *, expected1
			error stop 15
		end if
		if ( any(reshape(coarray[2], [8]) .ne. expected2) ) then
			print *, coarray[2]
			print *, expected2
			error stop 16
		end if

		threeD = 1
	end function
end
