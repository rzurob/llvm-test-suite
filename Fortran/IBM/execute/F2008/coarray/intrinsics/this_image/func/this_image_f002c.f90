!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : July 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This_image(caf, x) should always produce a result less
!*                               than or equal to num_images(), when no cobounds are specified.
!*                               Num_images must be set to 10.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	real(4), save :: caf[*]
	integer :: cnt

	cnt = this_image(caf, 1)
	if (cnt > num_images()) then
		print *, cnt, num_images()
		error stop 11
	end if
	sync all

	call sub1(num_images())
	call sub2()

contains

	subroutine sub2()
		real(8), save :: caf(2,2,2)[1,1,10:11,15:*]
		integer, allocatable :: lb(:)
		integer :: num, n

		lb = lcobound(caf)
		num = num_images()

		do i = 1, size(lb)
			n = this_image(caf, i)

			if (i <= 2) then
				if (n > num) then
					print *, n, num
					error stop 14
				end if
			else if (i <= 3) then
				if (n > 11) then
					print *, n
					error stop 15
				end if
			else if (i <= 4) then
				if (n > 19) then
					print *, n
					error stop 16
				end if
			else
				error stop 17
			end if
		end do
		sync all
	end subroutine
end


subroutine sub1(num)
	real(8), save :: caf(10)[0:1,*]
	integer, allocatable :: lb(:), ub(:)
	integer :: num, n

	lb = lcobound(caf)
	ub = ucobound(caf)

	if (size(lb) /= size(ub)) then
		print *, lb, ":", ub
		error stop 12
	end if

	do i = 1, size(lb)
		n = this_image(caf, i)

		if (n > num) then
			print *, n, num
			error stop 13
		end if
	end do
	sync all
end subroutine
