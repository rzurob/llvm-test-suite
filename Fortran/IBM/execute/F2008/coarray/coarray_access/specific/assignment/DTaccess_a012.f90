!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : DTaccess_a012.f
!*
!*  DATE                       : March 2011
!*
!*  DESCRIPTION
!*
!*  Assign module non-coarray variables to Derived Type coarray components
!*  (scalars and arrays of different kinds) and vice versa
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module modVAN

	type A
		integer :: i
		character(3) :: ch
		real :: r
		complex :: cx
		logical :: l
	end type

	integer, parameter :: intish = 12
	real, parameter :: realish = 3433.6454
	character(3), parameter :: charish = "BIT"
	complex, parameter :: plexish = (1.2,-1.1)
	logical, parameter :: logish = .true.

	type(A), save :: cafa[*], cafa10(4)[*]

contains

	subroutine modsub
		integer :: ivar, iarr(4)
		real :: rvar, rarr(4)
		character(3) :: chvar, charr(4)
		complex :: cxvar, cxarr(4)
		logical :: lvar, larr(4)

		call sub

		contains

			subroutine sub

				ivar = intish
				rvar = realish
				chvar = charish
				cxvar = plexish
				lvar = logish

				! assign values to coarrays
				cafa%i = ivar
				cafa%r = rvar
				cafa%ch = chvar
				cafa%cx = cxvar
				cafa%l = lvar

				cafa10%i = [1,5,2,ivar]
				cafa10(:)%r = rvar
				cafa10%ch = (/"PHI", "BOS", "VAN", "DET"/)
				cafa10(:)%cx = cxvar
				cafa10(:)%l = lvar


				if ( (cafa%i /= intish) .or. (cafa%r /= realish) .or. (cafa%ch /= charish) .or. (cafa%cx /= plexish) .or. (cafa%l .eqv. logish) ) then
					print *, cafa
					error stop 15
				end if

				if ( any(cafa10%i /= [1,5,2,ivar]) ) then
					print *, "expected", [1,5,2,ivar]
					print *, "actual", cafa10%i
					error stop 16
				end if

				if ( any(cafa10%r /= [rvar, rvar, rvar, rvar]) ) then
					print *, "expected", [rvar, rvar, rvar, rvar]
					print *, "actual", cafa10%r
					error stop 17
				end if

				if ( any(cafa10%ch <> ["PHI", "BOS", "VAN", "DET"]) ) then
					print *, "expected", ["PHI", "BOS", "VAN", "DET"]
					print *, "actual", cafa10%ch
					error stop 18
				end if

				if ( any(cafa10%cx .ne. [(cxvar, i = 1,4)]) ) then
					print *, "expected", [(cxvar, i = 1,4)]
					print *, "actual", cafa10%cx
					error stop 19
				end if

				if ( any(cafa10%l .neqv. [.true., .true., .true., .true.]) ) then
					print *, "expected", [.true., .true., .true., .true.]
					print *, "actual", cafa10%l
					error stop 19
				end if


				! assign values from coarrays
				ivar = cafa%i
				rvar = cafa%r
				chvar = cafa%ch
				cxvar = cafa%cx
				lvar = cafa%l

				iarr = cafa10%i
				rarr = cafa10%r
				charr = cafa10%ch
				cxarr = cafa10%cx
				larr = cafa10%l

				if ( (ivar /= intish) .or. (rvar /= realish) .or. (chvar /= charish) .or. (cxvar /= plexish) .or. (lvar .eqv. logish) ) then
					print *, ivar, rvar, chvar, cxvar, lvar
					error stop 25
				end if

				if ( any(iarr /= cafa10%i) ) then
					print *, iarr
					error stop 26
				end if
				if ( any(rarr /= cafa10%r) )  then
					print *, rarr
					error stop 27
				end if

				if ( any(charr /= cafa10%ch) )  then
					print *, charr
					error stop 28
				end if

				if ( any(cxarr /= cafa10%cx) )  then
					print *, cxarr
					error stop 29
				end if

				if ( any(larr .neqv. cafa10%l) )  then
					print *, ilarr
					error stop 30
				end if

			end subroutine

	end subroutine

end module


program main
	use :: modVAN

	call modsub
end
