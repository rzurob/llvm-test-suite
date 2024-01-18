!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : critical_f014b.f
!*
!*  PROGRAMMER                 : Francesco Cassullo
!*  DATE                       : February 2011
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test critical construct with a derived type
!*                               coarray dummy argument.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program DTDummyArgB
	real :: res
	
	type blk
		sequence
		integer :: left, right
	end type
	type (blk), save :: sample[*]
	
	interface
		real function calcDT(caf)
			type blk
				sequence
				integer :: left, right
			end type
			type (blk) :: caf[*]
		end function
	end interface
	
	res = calcDT(sample)
end


real function calcDT(caf)
	integer :: num
	
	type blk
		sequence
		integer :: left, right
	end type
	type (blk) :: caf[*]
	
	num = num_images()
	caf = blk(0, 0)
	sync all
	
	do n = 1, 100000
		critical
			if (this_image() == 1) then
				caf[2]%left = num
				caf[2]%right = num
			else
				if (caf[2]%left /= caf[2]%right) then
					print *, caf[2]%left, caf[2]%right
					error stop 15
				end if
			end if
		end critical
	end do
	
	sync all
	calcDT = 1.0
end function
