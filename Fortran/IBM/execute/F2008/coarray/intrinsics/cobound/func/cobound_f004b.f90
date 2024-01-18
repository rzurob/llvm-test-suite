!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : September 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test lcobound/ucobound with 2 arguments
!*                               produces consistent results across scopes.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC
	complex(4), save :: caf_mod[9:9,8:8,7:7,5:6,2:*]

contains

	subroutine sub1(size)
		integer :: size
		integer :: arr_lo(size), arr_hi(size)

		do i = 1, size
			arr_lo(i) = lcobound(COARRAY=caf_mod, DIM=i)
			arr_hi(i) = ucobound(COARRAY=caf_mod, DIM=i)
		end do
		print *, arr_lo, ":", arr_hi
		sync all
	end subroutine
end module


program main
	use modFDC
	character(8), save :: caf_main[9:9,8:8,7:7,5:6,2:*]
	integer :: arr1(5), arr2(5), x
	integer, parameter :: size = 5

	do i = 1, size
		arr1(i) = lcobound(COARRAY=caf_main, DIM=i)
		arr2(i) = ucobound(COARRAY=caf_main, DIM=i)
	end do
	print *, arr1, ":", arr2
	sync all

	x = fun1(size)
	call sub1(size)
	call sub2(size)

contains

	integer function fun1(size)
		integer(2), save :: caf_int[9:9,8:8,7:7,5:6,2:*]
		integer :: size
		integer :: arr_lo(size), arr_hi(size)

		do i = 1, size
			arr_lo(i) = lcobound(COARRAY=caf_int, DIM=i)
			arr_hi(i) = ucobound(COARRAY=caf_int, DIM=i)
		end do
		print *, arr_lo, ":", arr_hi
		sync all
	end function

end


subroutine sub2(size)
	logical(1), save :: caf_ext[9:9,8:8,7:7,5:6,2:*]
	integer :: size
	integer :: arr1(size), arr2(size)

	do i = 1, size
		arr1(i) = lcobound(COARRAY=caf_ext, DIM=i)
		arr2(i) = ucobound(COARRAY=caf_ext, DIM=i)
	end do
	print *, arr1, ":", arr2
	sync all
end subroutine
