!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : September 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test lcobound/ucobound with 1 argument
!*                               with a dummy argument coarray.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer(1), save :: caf(2,2)[2,1,*]
	integer, parameter :: n = 3
	integer :: arr1(3), arr2(3)

	interface
		subroutine sub1(caf, n)
			integer(1) :: caf(2,2)[2,1,*]
			integer :: n
		end subroutine
	end interface

	arr1 = lcobound(caf)
	arr2 = ucobound(caf)

	if ( any(arr1 .ne. [1,1,1]) ) then
		print *, arr1
		error stop 11
	end if
	if ( any(arr2 .ne. [2,1,3]) ) then
		print *, arr2
		error stop 12
	end if
	sync all

	call sub1(caf, n)

end


subroutine sub1(caf, n)

	integer(1) :: caf(2,2)[2,1,*]
	integer :: n
	integer :: arr1(n), arr2(n)

	arr1 = lcobound(caf)
	arr2 = ucobound(caf)

	if ( any(arr1 .ne. [1,1,1]) ) then
		print *, arr1
		error stop 13
	end if
	if ( any(arr2 .ne. [2,1,3]) ) then
		print *, arr2
		error stop 14
	end if
	sync all

end subroutine

