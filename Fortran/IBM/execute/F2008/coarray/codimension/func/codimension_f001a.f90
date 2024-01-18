!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : October 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test codimension attribute with a static coarray.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC

contains
	subroutine sub2()
		logical, save, codimension[0:*] :: caf4
		integer :: a, b

		a = lcobound(caf4, 1)
		b = ucobound(caf4, 1)
		if (a /= 0) then
			print *, a
			error stop 20
		end if
		if ( b /= (num_images() - 1) ) then
			print *, b, num_images() - 1
			error stop 21
		end if

		caf4 = .false.
		if ( caf4 ) then
			print *, caf4
			error stop 22
		end if
	end subroutine
end module


program main

	use modFDC
	real, save, codimension[*] :: caf1
	integer :: a, b

	a = lcobound(caf1, 1)
	b = ucobound(caf1, 1)
	if (a /= 1) then
		print *, a
		error stop 11
	end if
	if (b /= num_images()) then
		print *, b, num_images()
		error stop 12
	end if
	sync all

	caf1 = 10.5
	if (caf1 /= 10.5) then
		print *, caf1, this_image()
		error stop 13
	end if

	a = fun1()
	call sub1()
	call sub2()

contains

	integer function fun1()
		integer, save, codimension[*] :: caf2(2,2)

		a = lcobound(caf2, 1)
		b = ucobound(caf2, 1)
		if (a /= 1) then
			print *, a
			error stop 14
		end if
		if (b /= num_images()) then
			print *, b, num_images()
			error stop 15
		end if

		num = this_image()
		caf2 = num
		sync all
		if ( any(caf2 .ne. reshape([num, num, num, num], [2,2])) ) then
			print *, caf2, num
			error stop 16
		end if

		fun1 = 0
	end function
end


subroutine sub1()
	integer, save, codimension[1,*], dimension(10) :: caf3
	integer, allocatable :: a(:), b(:)
	integer :: num

	num = num_images()
	a = lcobound(caf3)
	b = ucobound(caf3)
	if ( any(a .ne. [1,1]) ) then
		print *, a
		error stop 17
	end if
	if ( any(b .ne. [1,num]) ) then
		print *, b, num_images()
		error stop 18
	end if
	sync all

	caf3 = 3
	if ( any(caf3 .ne. [3,3,3,3,3,3,3,3,3,3]) ) then
		print *, caf3
		error stop 19
	end if
end subroutine