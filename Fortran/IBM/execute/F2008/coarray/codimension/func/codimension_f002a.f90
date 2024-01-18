!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : codimension_f002a.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : October 2010
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test codimension attribute statement with a static coarray.
!*                            
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC

contains
	subroutine sub2()
		logical, save :: caf4
		real :: a, b
		codimension caf4[5,4,3,2,*]
		
		a = ceiling(force(4)) - force(4)
		if (a > 0.5) then
			caf4 = .true. 
		else
			caf4 = .false.
		end if
		if (.not. caf4 ) then
			print *, caf4
			error stop 20
		end if
	end subroutine
end module


program main
	
	use modFDC
	real, save :: caf1
	integer :: a, b, num
	codimension caf1[*]
	
	num = num_images()
	a = lcobound(caf1, 1)
	b = ucobound(caf1, 1)
	if (a /= 1) then
		print *, a
		error stop 11
	end if
	if (b /= num) then
		print *, b, num
		error stop 12
	end if
	
	caf1 = force(5)
	if (caf1 /= 49.0) then
		print *, caf1, this_image()
		error stop 13
	end if
	
	a = fun1()
	call sub1(num)
	call sub2()
	
contains

	integer function fun1()
		integer, save :: caf2(2,3)
		codimension caf2[10:*]
		
		a = lcobound(caf2, 1)
		b = ucobound(caf2, 1)
		if (a /= 10) then
			print *, a
			error stop 14
		end if
		if (b /= (num + 9)) then
			print *, b, num
			error stop 15
		end if
		
		do i = 1, 3
			caf2(:,i) = floor(force(i))
		end do
		sync all
		if ( any(caf2 .ne. reshape([9,9,19,19,29,29], [2,3])) ) then
			print *, caf2
			error stop 16
		end if
		
		fun1 = 0
	end function
end


subroutine sub1(num)
	complex, save, dimension(10) :: caf3
	integer :: num
	codimension caf3[0:2,1:3,2:*]
	
	caf3 = (-1.0, 2.0)
	sync all
	do i = 1, 10
		if (caf3(i) /= (-1.0, 2.0)) then
			print *, caf3
			error stop 18
		end if
	end do
end subroutine


real function force(mass)
	integer :: mass
	real, parameter :: accel = 9.8

	force = mass * accel
end function