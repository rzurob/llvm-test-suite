!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : October 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test codimension attribute statement with a dummy coarray.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module modFDC

contains
	subroutine sub1(cafa, cafb)
		integer*8 :: cafa, cafb
		codimension cafa[*], cafb[2,5:*]

		cafa = 66
		cafb = 99

		if (cafa /= 66) then
			print *, cafa
			error stop 21
		end if
		sync all
		if (cafb /= 99) then
			print *, cafb
			error stop 22
		end if
		sync all

		cafb = cafa + cafb - 78
		if (cafb /= 87 .or. cafa /= 66) then
			print *, cafb, cafa
			error stop 23
		end if
	end subroutine
end module


program main
	use modFDC
	integer*8, save, codimension[*] :: caf1
	integer*2, save :: caf2
	integer*4, save, codimension[-10:-9,-1:3,0:*] :: caf3(9)
	integer*8, save :: caf4
	integer :: a

	codimension caf4[1,1,1,1,1,1,1,1,*], caf2[1:*]
	dimension caf2(2,2)

	interface
		integer function fun1(c1, c2)
			integer*2, dimension(2,2) :: c1
			integer*4 :: c2(9)
			codimension c1[1:*], c2[-10:-9,-1:3,0:*]
		end function
	end interface

	a = fun1(caf2, caf3)
	call sub1(caf1, caf4)
end


integer function fun1(cafa, cafb)
	integer*2, dimension(2,2) :: cafa
	integer*4 :: cafb(9)

	codimension cafa[1:*], cafb[-10:-9,-1:3,0:*]

	cafb = -1
	if (this_image() == 1) then
		cafb(5:9) = 0
	end if
	sync all

	if (this_image() == 1) then
		if ( any(cafb .ne. [-1,-1,-1,-1,0,0,0,0,0]) ) then
			print *, cafb
			error stop 11
		end if
	else
		if ( any(cafb .ne. [-1,-1,-1,-1,-1,-1,-1,-1,-1]) ) then
			print *, cafb
			error stop 12
		end if
	end if


	cafa = 0
	if (this_image() == 1) then
		cafa(1,:) = 5
	end if
	if (this_image() == 2) then
		cafa(2,:) = 10
	end if

	if (this_image() == 1) then
		if ( any(cafa .ne. reshape([5,0,5,0], [2,2])) ) then
			print *, cafa
			error stop 13
		end if
	else if (this_image() == 2) then
		if ( any(cafa .ne. reshape([0,10,0,10], [2,2])) ) then
			print *, cafa
			error stop 14
		end if
	else
		if ( any(cafa .ne. reshape([0,0,0,0], [2,2])) ) then
			print *, cafa
			error stop 15
		end if
	end if

	fun1 = 1
end function
