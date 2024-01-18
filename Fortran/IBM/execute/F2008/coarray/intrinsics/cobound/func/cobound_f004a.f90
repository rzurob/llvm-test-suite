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
!*                               produces consistent results across scopes.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC
	complex(4), save :: caf_mod[1,1:2,0:1,-1:*]
	integer :: arr_lo(4), arr_hi(4)

contains

	subroutine sub1()
		arr_lo = lcobound(caf_mod)
		arr_hi = ucobound(caf_mod)
		print *, arr_lo, ":", arr_hi
		sync all
	end subroutine
end module


program main
	use modFDC
	real(8), save :: caf_main(2,2)[1,1:2,0:1,-1:*]
	integer :: arr1(4), arr2(4), x

	arr1 = lcobound(caf_main)
	arr2 = ucobound(caf_main)
	print *, arr1, ":", arr2
	sync all

	x = fun1(0, 1)
	call sub1()
	call sub2(0, 1)

contains

	integer function fun1(m, n)
		integer :: m, n
		integer(1), save :: caf_int[1,1:2,m:n,-1:*]

		arr1 = lcobound(caf_int)
		arr2 = ucobound(caf_int)
		print *, arr1, ":", arr2
		sync all
	end function

end


subroutine sub2(m, n)
	integer :: m, n
	logical(2), save :: caf_ext(2,2)[1,1:2,m:n,-1:*]
	integer :: arr1(4), arr2(4)

	arr1 = lcobound(caf_ext)
	arr2 = ucobound(caf_ext)
	print *, arr1, ":", arr2
	sync all
end subroutine