!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : March 2011
!*
!*  DESCRIPTION
!*
!*  Assign a non-coarray variable to a Derived Type coarray allocatable component
!*  and vice versa
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module modOTT
	type coarray_cluster
		integer(1), allocatable :: i1
		integer(2), allocatable :: i2(:)
		integer(4), allocatable :: i4
		integer(8), allocatable :: i8(:,:)
	end type
	type(coarray_cluster), save, codimension[*] :: CAF
end module


program main
	use modOTT
	implicit none

	call sub0()

contains

	subroutine sub0()

		integer(1), parameter :: mid1 = 1_1
		integer(2), parameter :: mid2 = 499_2
		integer(4), parameter :: mid4 = 10807551_4
		integer(8), parameter :: mid8 = -800851337740_8

		integer(1) :: v1, iatmp1(4)
		integer(2) :: v2, iatmp2(4)
		integer(4) :: v4, iatmp4(4)
		integer(8), allocatable :: v8, iatmp8(:)

		v1 = mid1
		v2 = mid2
		v4 = mid4
		v8 = mid8
		iatmp1 = [0,1,2,3]
		iatmp2 = [2,4,6,8]
		iatmp4 = [3,6,9,12]
		allocate(iatmp8(4))
		iatmp8 = [5,7,11,13]

		! assign to coarray
		allocate(CAF%i1)
		allocate(CAF%i2(4), source = [integer(2) :: 0,0,0,0])
		CAF%i1 = v1
		CAF%i2(1:2) = iatmp2(1:2)
		CAF%i2(3:) = iatmp4(3:4)
		CAF%i4 = v4
		CAF%i8 = reshape(iatmp8, [2,2])


		if ( CAF%i1 /= mid1) then
			print *, "actual", CAF%i1
			print *, "expected", mid1
			error stop 21
		end if

		if ( any(CAF%i2 .ne. [integer(2) :: iatmp2(1:2), iatmp4(3:4)]) ) then
			print *, "actual", CAF%i2
			print *, "expected", [integer(2) :: iatmp2(1:2), iatmp4(3:4)]
			error stop 22
		end if

		if (CAF%i4 <> mid4) then
			print *, "actual", CAF%i4
			print *, "expected", mid4
			error stop 23
		end if

		if ( any(CAF%i8 .ne. reshape(iatmp8, [2,2])) ) then
			print *, "actual", CAF%i8
			print *, "expected", iatmp8
			error stop 24
		end if


		! assign from coarray
		deallocate(CAF%i1)
		deallocate(CAF%i2)
		deallocate(CAF%i4)
		deallocate(CAF%i8)
		CAF%i1 = mid1
		CAF%i2 = [integer(2) :: mid1, mid2, mid4, mid8]
		allocate(CAF%i4)
		CAF%i4 = mid4
		allocate(CAF%i8(2,2))
		CAF%i8 = reshape([integer(8) :: mid1, mid2, mid4, mid8], [2,2])

		v1 = CAF%i1
		iatmp2 = CAF%i2
		v4 = CAF%i4
		iatmp8 = reshape(CAF%i8, [4])

		if (v1 /= mid1) then
			print *, "actual", v1
			print *, "expected",  mid1
			error stop 31
		end if

		if ( any(iatmp2 /= [integer(2) :: mid1, mid2, mid4, mid8]) ) then
			print *, "actual", iatmp2
			print *, "expected", mid1, mid2, mid4, mid8
			error stop 32
		end if

		if (v4 <> mid4) then
			print *, "actual", v4
			print *, "expected", mid4
			error stop 33
		end if

		if ( any(iatmp8 /= [integer(8) :: mid1, mid2, mid4, mid8]) ) then
			print *, "actual", iatmp8
			print *, "expected", mid1, mid2, mid4, mid8
			error stop 34
		end if

		deallocate(CAF%i1)
		deallocate(CAF%i2)
		deallocate(CAF%i4)
		deallocate(CAF%i8)

	end subroutine

end
