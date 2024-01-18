!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : critical_f001.f
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
!*  DESCRIPTION                : Very simple critical construct sync
!*                               test of a corray object.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, save   :: x[*]
	integer, parameter :: index = 1
	integer, parameter :: loop_cnt = 10
	integer :: n, tmp

	n = num_images()
	x = 0
	sync all

	critical
		do i = 1, loop_cnt
			tmp = x[index] + 1
			x[index] = tmp
		end do
	end critical

	sync all

	if (x[index] /= (loop_cnt * n)) then
		print *, x[index]
		print *, loop_cnt * n
		error stop 10
	end if

	do i = index + 1, n
		if (x[i] /= 0) then
			print *, x[i], "on image", i
			error stop 11
		end if
	end do

end
