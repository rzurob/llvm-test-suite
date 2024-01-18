!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : February 2011
!*  ORIGIN                     : AIX Compiler Development,
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

program DTDummyArgA
	integer :: num
	real :: res

	type sfx
		sequence
		real(8) :: left, right
	end type
	type (sfx), save :: sample[*], dopple(100)[*]

	interface
		real function opDT(caf, cafar)
			type sfx
				sequence
				real(8) :: left, right
			end type
			type (sfx) :: caf[*], cafar(100)[*]
		end function
	end interface

	res = opDT(sample, dopple)
end


real function opDT(caf, cafar)
	type sfx
		sequence
		real(8) :: left, right
	end type
	type (sfx) :: caf[*], cafar(100)[*]
	integer, save :: iter[*]
	integer :: num

	num = num_images()
	iter = 1
	caf = sfx(1.0, 1.0)
	cafar = sfx(1.0, 1.0)
	sync all

	critical
		caf[1]%left = 2 ** iter[1]
		caf[1]%right = caf[1]%right / 2
	end critical

	sync all
	if (caf[1]%left /= 2) then
		print *, caf[1]%left
		print *, 2 ** num
		error stop 16
	end if

	if ( caf[1]%right /= (1 / (2 ** num)) ) then
		print *, caf[1]%right
		print *, 1 / (2 ** num)
		error stop 17
	end if

	do i = 1, 1000
		critical
			if (this_image() == 1) then
				cafar(:)%left = real(num, 8)
			else
				if (cafar(1)%left /= cafar(100)%left) then
					print *, cafar(1)%left, cafar(100)%left
					error stop 18
				end if
			end if
		end critical
	end do

	opDT = 1.0
end function
