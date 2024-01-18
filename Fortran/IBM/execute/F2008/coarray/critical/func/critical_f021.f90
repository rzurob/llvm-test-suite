!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : critical_f021.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : January 2011
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Verify the value of lcobound()/ucobound() called
!*                               within a critical construct.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, save :: caf[0:1,1:1,2:3,0:*], caf2[1:*]
	integer :: i, j, k, l, tmp

	caf2 = 0
	caf = 0
	sync all
	
	critical
		caf2[lcobound(COARRAY=caf, DIM=2)] = caf2[lcobound(COARRAY=caf, DIM=2)] + 1
		
		do i = lcobound(caf, DIM=1), lcobound(caf, DIM=1)
			do j = lcobound(caf, DIM=2), lcobound(caf, DIM=2)
				do k = lcobound(caf, DIM=3), lcobound(caf, DIM=3)
					do l = lcobound(caf, DIM=4), lcobound(caf, DIM=4)
						tmp = 1 + caf[0,1,2,0]
						caf[0,1,2,0] = tmp
					end do
				end do
			end do
		end do
	end critical
	
	sync all
	
	if (caf2[lcobound(caf, DIM=2)] /= num_images()) then
		print *, lcobound(caf, DIM=2)
		print *, "actual", caf2[lcobound(caf, DIM=2)]
		print *, "expected", num_images()
		error stop 15
	end if

	if (caf[0,1,2,0] /= num_images()) then
		print *, caf[0,1,2,0]
		error stop 16
	end if
	
end
