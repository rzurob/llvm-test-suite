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
!*  DESCRIPTION                : Test codimension attribute with a dummy coarray.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	real, save, codimension[*] :: caf1
	integer, save :: caf2(3,3,1)[*]
	real, save, codimension[2,2,*], dimension(1) :: caf3
	complex, save, codimension[1:2,0:*] :: caf4
	integer :: a, b

	real, save :: caf5[-10:*]
	integer, save :: caf6(3,3,1)[1:5,*]
	real, save :: caf7(1)[0:1,-1:0,10:*]
	complex, save, codimension[*] :: caf8

	interface
		subroutine sub1(c1, c2)
			real, codimension[2,2,*], dimension(1) :: c1
			complex, codimension[-1:*] :: c2
		end subroutine
	end interface

	a = fun1(caf1, caf2)
	a = fun1(caf5, caf6)
	call sub1(caf3, caf4)
	call sub1(caf7, caf8)

contains

	integer function fun1(cafa, cafb)
		real, codimension[*] :: cafa
		integer, codimension[1:2,0:*] :: cafb(3,3,1)
		real :: a, b
		integer :: c(2), d(2)

		!### Test Bounds
		a = lcobound(cafa, 1)
		b = ucobound(cafa, 1)
		c = lcobound(cafb)
		d = ucobound(cafb)
		if (a /= 1) then
			print *, a
			error stop 11
		end if
		if (b /= num_images()) then
			print *, b, num_images()
			error stop 12
		end if
		if ( any(c .ne. [1,0]) ) then
			print *, c
			error stop 13
		end if
		if ( d(1) /= 2 ) then
			print *, d
			error stop 14
		end if
		sync all

		!### Test assignment
		cafa = 10.0
		cafb = int(cafa) + 1
		sync all
		if (cafa /= 10.0) then
			print *, cafa
			error stop 15
		end if
		if ( any(cafb .ne. reshape([11,11,11,11,11,11,11,11,11], [3,3,1])) ) then
			print *, caf2, num
			error stop 16
		end if

		fun1 = 1
	end function
end


subroutine sub1(caf3, caf4)
	real, codimension[2,2,*], dimension(1) :: caf3
	complex, codimension[-1:*] :: caf4
	integer :: num, a, b, c(3)

	!### Test Bounds
	num = num_images()
	a = lcobound(caf4, 1)
	b = ucobound(caf4, 1)
	if (a /= -1) then
		print *, a
		error stop 17
	end if
	if (b /= (num - 2)) then
		print *, b, num - 2
		error stop 18
	end if

	c = lcobound(caf3)
	if ( any(c .ne. [1,1,1]) ) then
		print *, c
		error stop 19
	end if
	sync all

	!### Test assignment
	caf3 = 1.0
	if ( any(caf3 .ne. [1.0]) ) then
		print *, caf3
		error stop 20
	end if
end subroutine
