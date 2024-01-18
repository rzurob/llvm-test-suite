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
!*  DESCRIPTION                : This_image(caf) and this_image(caf,x) must match
!*				 cobounds of coarray declarations.
!*				 Must run with 12 images.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, save :: caf[3:5,*], caf2[2,9:10,7:*]
	integer :: num, num2
	integer, allocatable :: arr(:)

	allocate(arr(2))
	arr = this_image(caf)
	if ((arr(1) < 3) .or. (arr(1) > 5)) then
		print *, arr
		error stop 11
	end if
	if ((arr(2) < 1) .or. (arr(2) > 4)) then
		print *, arr
		error stop 12
	end if
	deallocate(arr)
	sync all

	num = this_image(caf, 1)
	num2 = this_image(caf, 2)
	if ((num < 3) .or. (num > 5)) then
		print *, num
		error stop 13
	end if
	if ((num2 < 1) .or. (num2 > 4)) then
		print *, num2
		error stop 14
	end if

!=========================================
!=========================================

	allocate(arr(3))
	arr = this_image(caf2)
	print *, arr
	if ((arr(1) < 1) .or. (arr(1) > 2)) then
		print *, arr
		error stop 21
	end if
	if ((arr(2) < 9) .or. (arr(2) > 10)) then
		print *, arr
		error stop 22
	end if
	if ((arr(3) < 7) .or. (arr(3) > 9)) then
		print *, arr
		error stop 23
	end if
	deallocate(arr)

	allocate(arr(3))
	do i = 1, size(arr)
		arr(i) = this_image(caf2, i)
	end do
	sync all
	if ((arr(1) < 1) .or. (arr(1) > 2)) then
		print *, arr
		error stop 24
	end if
	if ((arr(2) < 9) .or. (arr(2) > 10)) then
		print *, arr
		error stop 25
	end if
	if ((arr(3) < 7) .or. (arr(3) > 9)) then
		print *, arr
		error stop 26
	end if

end
