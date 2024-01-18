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
!*  DESCRIPTION                : Test lcobound/ucobound with 2/3 arguments
!*                               using a use associated coarray. Test Kind type.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC
	logical, save :: caf[1:2,2,0:0,0:0,2,1:*]
end module


program main
	use modFDC
	integer, parameter :: n = 6

	call sub_kind1()
	call sub_kind2()
	call sub_kind4()
	call sub_kind8()

contains

	subroutine sub_kind1()
		integer(1) :: alo(n), ahi(n)

		do i = 1, n
			alo(i) = lcobound(caf, i, 1)
			ahi(i) = ucobound(caf, i, 1)
		end do

		if ( any(alo .ne. [1_1,1_1,0_1,0_1,1_1,1_1]) ) then
			print *, alo
			error stop 11
		end if
		if ( any(ahi .ne. [2_1,2_1,0_1,0_1,2_1,3_1]) ) then
			print *, ahi
			error stop 12
		end if
		sync all
	end subroutine


	subroutine sub_kind2()
		integer, parameter :: k = 2
		integer(k) :: alo(n), ahi(n)

		do i = 1, n
			alo = lcobound(COARRAY=caf, KIND=k)
			ahi = ucobound(COARRAY=caf, KIND=k)
		end do

		if ( any(alo .ne. [1_2,1_2,0_2,0_2,1_2,1_2]) ) then
			print *, alo
			error stop 13
		end if
		if ( any(ahi .ne. [2_2,2_2,0_2,0_2,2_2,3_2]) ) then
			print *, ahi
			error stop 14
		end if
		sync all
	end subroutine


	subroutine sub_kind4()
		integer(4) :: alo(n), ahi(n)

		do i = 1, n
			alo(i) = lcobound(caf, i, KIND=kind(alo))
			ahi(i) = ucobound(caf, i, KIND=kind(ahi))
		end do

		if ( any(alo .ne. [1_4,1_4,0_4,0_4,1_4,1_4]) ) then
			print *, alo
			error stop 15
		end if
		if ( any(ahi .ne. [2_4,2_4,0_4,0_4,2_4,3_4]) ) then
			print *, ahi
			error stop 16
		end if
		sync all
	end subroutine


	subroutine sub_kind8()
		integer, parameter :: k = 8
		integer(k) :: alo(n), ahi(n)

		do i = 1, n
			alo = lcobound(COARRAY=caf, KIND=k)
			ahi = ucobound(COARRAY=caf, KIND=k)
		end do

		if ( any(alo .ne. [1_8,1_8,0_8,0_8,1_8,1_8]) ) then
			print *, alo
			error stop 17
		end if
		if ( any(ahi .ne. [2_8,2_8,0_8,0_8,2_8,3_8]) ) then
			print *, ahi
			error stop 18
		end if
		sync all
	end subroutine

end
