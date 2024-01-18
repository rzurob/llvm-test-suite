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
!*  DESCRIPTION                : This_image(caf, n) should produce a value that
!*				 matches the nth element from this_image(caf).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, save :: caf[*], caf2[-1:*], caf3[1,1:1,-1:0,0:2,*]
	integer :: count, num
	integer, allocatable :: arr(:), lb(:)

!##### corank=1
	allocate(arr(1))
	arr = this_image(caf)
	count = size(arr)
	lb = lcobound(caf)

	if (count /= size(lb)) then
		print *, count, size(lb)
		error stop 11
	end if

	do i = 1, count
		num = this_image(caf, i)
		if (num /= arr(i)) then
			print *, num, arr(i)
			error stop 12
		end if
	end do
	deallocate(arr)


!##### corank=2
	allocate(arr(2))
	arr = this_image(caf2)
	count = size(arr)

	do i = 1, count
		num = this_image(caf2, i)
		if (num /= arr(i)) then
			print *, num, arr(i)
			error stop 13
		end if
	end do
	deallocate(arr)


!##### corank=3
	allocate(arr(5))
	arr = this_image(caf3)
	count = size(arr)

	do i = 1, count
		num = this_image(caf3, i)
		if (num /= arr(i)) then
			print *, num, arr(i)
			error stop 14
		end if
	end do
	deallocate(arr)

end
