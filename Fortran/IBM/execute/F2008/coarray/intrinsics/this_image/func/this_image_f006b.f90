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
!*  DESCRIPTION                : This_image() should produce consistent results
!*				 throughout the program.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	complex*8, save :: caf[*]
	integer :: s1, s2
	integer, allocatable :: arr(:), arr2(:)

	arr = this_image(caf)
	s1 = size(arr)
	arr2 = this_image(caf)
	s2 = size(arr2)


	if (s1 /= s2) then
		print *, s1, s2
		error stop 11
	end if

!!!compare results
	do i = 1, s1
		if (arr(i) /= arr2(i)) then
			print *, arr(i), arr2(i)
			error stop 12
		end if
	end do

	call sub0(caf)

end


subroutine sub0(caf)

	complex*8 :: caf[*]
	integer :: num1, num2
	integer, allocatable :: lb(:), arr1(:), arr2(:)

	lb = lcobound(caf)
	if (size(lb) > 7) error stop 21

	allocate( arr1(size(lb)) )
	allocate( arr2(size(lb)) )
	do i = 1, size(lb)
		arr1(i) = this_image(caf, i)
	end do
	do i = 1, size(lb)
		arr2(i) = this_image(caf, i)
	end do

!!!compare results
	do i = 1, size(lb)
		if (arr1(i) /= arr2(i)) then
			print *, arr1(i), arr2(i)
			error stop 22
		end if
	end do

	deallocate(arr1, arr2)

end subroutine
