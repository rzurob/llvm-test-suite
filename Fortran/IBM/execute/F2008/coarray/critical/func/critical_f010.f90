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
!*  DESCRIPTION                : Test critical construct in a module procedure
!*                               using an assumed shape dummy coarray.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modQOTSA
	integer, parameter :: mux = 2

contains
	subroutine sub0(caf, img, tmp)
		integer(4) :: caf(:)[*]
		integer(4) :: tmp(:)

		critical
			tmp = caf[img] * mux
			print *, tmp
			caf[img] = tmp
		end critical

	end subroutine

end module


program main
	use modQOTSA
	integer(4), save :: caf(4)[*], caf2(2)[*]
	integer(4) :: total(4), tmp(4)
	integer :: i

	caf = [1,2,3,4]
	caf2 = [-10, -10]

	do i = 1, 4
		total(i) = caf(i)[1] * (mux ** num_images())
	end do
	sync all

!!!First Run
	call sub0(caf, 1, tmp)
	sync all

	if ( any(caf[1] .ne. total(i)) ) then
		print *, "actual", caf[1]
		print *, "expected", total
		error stop 15
	end if

!!!Second Run
	total(1:2) = caf2 * mux * mux
	sync all
	call sub0(caf2, num_images(), tmp)
	sync all

	if ( any(caf2[num_images()] .ne. total(1:2)) ) then
		print *, "actual", caf2[num_images()]
		print *, "expected", total(1:2)
		error stop 15
	end if
end
