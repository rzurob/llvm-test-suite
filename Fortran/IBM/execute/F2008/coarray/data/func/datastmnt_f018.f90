!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : November 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test initialization with DATA statements of derived type coarray arrays (components).
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	type point
		integer :: x, y, z
		real :: t
	end type
	type (point), save :: p1(5)[*], p2(10)[1,1:2,3,-1:1,5:9,1:*]
	type (point), save :: p3(2,2)[0:2,0:2,0:*]

	data p1%x/1, 3, 5, 7, 9/
	data p1%y/2, 4, 6, 8, 10/
	data p1%z/-9, -8, -7, -6, -5/
	data p1%t/1.5, 2.5, 5.25, 19.5, 3.0/


	data p2(1:5)%x, p2(6:10)%x /5*1, 5*0/
	data p2(1:1)%y /1/, p2(2:10)%y /9*0/
	data p2(1:3)%z, p2(4:6)%z, p2(7:10)%z /3*1, 3*0, 4*-1/
	data p2(1:8)%t /8*1.0/, p2(9:10)%t /0.0, -1.0/


	data p3(1,1)%x, p3(1,1)%y, p3(1,1)%z, p3(1,1)%t /3*10, 44.4/
	data p3(1,2)%x /1/, p3(1,2)%y /2/, p3(1,2)%z /3/, p3(1,2)%t /4.0/
	data p3(2,1)%x, p3(2,1)%y, p3(2,1)%z, p3(2,1)%t /6, 2*9, 12.5/
	data p3(2,2)%x, p3(2,2)%y, p3(2,2)%z, p3(2,2)%t /2*1, 0, 0.0/

	x = f1(p1, p2, p3)

contains

	integer function f1(pa, pb, pc)
		type (point) :: pa(5)[3,3,3,*], pb(10)[*]
		type (point) :: pc(2,2)[5,10,1:*]

		if ( any(p1%x .ne. [1,3,5,7,9]) ) then
			print *, p1%x
			error stop 21
		end if
		if ( any(p1%y .ne. [2,4,6,8,10]) ) then
			print *, p1%y
			error stop 22
		end if
		if ( any(p1%z .ne. [-9,-8,-7,-6,-5]) ) then
			print *, p1%z
			error stop 23
		end if
		if ( any(p1%t .ne. [1.5,2.5,5.25,19.5,3.0]) ) then
			print *, p1%t
			error stop 24
		end if


		if ( any(p2%x .ne. [1,1,1,1,1,0,0,0,0,0]) ) then
			print *, p2%x
			error stop 31
		end if
		if ( any(p2%y .ne. [1,0,0,0,0,0,0,0,0,0]) ) then
			print *, p2%y
			error stop 32
		end if
		if ( any(p2%z .ne. [1,1,1,0,0,0,-1,-1,-1,-1]) ) then
			print *, p2%z
			error stop 33
		end if
		if ( any(p2%t .ne. [1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.0,-1.0]) ) then
			print *, p2%t
			error stop 34
		end if


		if ( any(reshape(p3%x, [4]) .ne. [10,6,1,1]) ) then
			print *, reshape(p3%x, [4])
			error stop 41
		end if
		if ( any(reshape(p3%y, [4]) .ne. [10,9,2,1]) ) then
			print *, reshape(p3%y, [4])
			error stop 42
		end if
		if ( any(reshape(p3%z, [4]) .ne. [10,9,3,0]) ) then
			print *, reshape(p3%z, [4])
			error stop 43
		end if
		if ( any(reshape(p3%t, [4]) .ne. [44.4,12.5,4.0,0.0]) ) then
			print *, reshape(p3%t, [4])
			error stop 44
		end if

		f1 = 1
	end function
end
