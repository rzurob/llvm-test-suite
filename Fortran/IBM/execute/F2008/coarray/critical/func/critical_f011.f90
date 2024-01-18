!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : critical_f011.f
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
!*  DESCRIPTION                : Test critical construct in an external procedure
!*                               using an assumed size dummy coarray.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	real(4), save :: caf(4)[*], caf2(2,3)[*]
	real(4) :: num(4), num2(6)
	integer :: res
	
	interface
		real(4) function fun1(caf, n)
			real(4):: caf(*)[*]
			integer :: n
		end function
	end interface
	
	caf = 0.0
	caf2 = 0.0
	num = num_images()
	num2 = [num(1), num(1), num(1), num(1), num(1), 0.0]
	sync all
	
	res = fun1(caf, 4)
	res = fun1(caf2, 5)
	
	if (this_image() == 1) then
		if ( any(caf .ne. num) ) then
			print *, caf
			print *, num
			error stop 14
		end if
		
		if ( any(reshape(caf2, (/6/)) .ne. num2) ) then
			print *, caf2
			print *, num2
			error stop 15
		end if
	else
		if ( any(caf .ne. [0.0,0.0,0.0,0.0]) ) then
			print *, caf
			print *, this_image()
			error stop 16
		end if
		
		if ( any(reshape(caf2, (/6/)) .ne. [0.0,0.0,0.0,0.0,0.0,0.0]) ) then
			print *, caf
			print *, this_image()
			error stop 17
		end if
	end if
end

real(4) function fun1(caf, n)
	real(4) :: caf(*)[*], tmp, tmp2
	integer :: n
	
	do i = 1, n
		critical
			tmp = caf(i)[1] + 1
			call sleep_(1)
			tmp2 = caf(i)[1]
			caf(i)[1] = tmp
		end critical
	end do
	
	fun1 = 0
	sync all
end function
