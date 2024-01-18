!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : codimension_f003b.f
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
!*  DESCRIPTION                : Test codimension attribute vs entity declaration with a dummy coarray.
!*                            
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	real(4), save, codimension[1,2,3,*] :: caf1[*]
	integer, save, dimension(3), codimension[0:*] :: caf2[2,2,2,4,*]
	
	interface
		integer function fun1(c)
			real(4), codimension[1,2,3,*] :: c[*]
		end function
	end interface
	
	caf1 = 15.5
	a = fun1(caf1)
	call sub1(caf2)
	
contains

	subroutine sub1(cafb)
		integer, codimension[-1:1,0:*] :: cafb(3)[1,2,3,*]
		integer, allocatable :: a(:), b(:)
		
		a = lcobound(cafb)
		b = ucobound(cafb)
		print *, a, ":", b
		sync all
		
		cafb = 9
		cafb[1,1,1,1] = -1
		cafb[1,2,2,2] = 0
		
		if (this_image() == 1) then
			if ( any(cafb .ne. [-1,-1,-1]) ) then
				print *, cafb
				error stop 21
			end if
		else if (this_image() == 10) then
			if ( any(cafb .ne. [0,0,0]) ) then
				print *, cafb
				error stop 22
			end if
		else
			if ( any(cafb .ne. [9,9,9]) ) then
				print *, cafb
				error stop 23
			end if
		end if
		sync all
	end subroutine
end


integer function fun1(cafa)
	real(4), codimension[1,2,3,*] :: cafa[*]
	integer, allocatable :: a(:), b(:)
	
	a = lcobound(cafa)
	b = ucobound(cafa)
	
	if ((size(a) /= 1) .or. (size(b) /= 1)) then
		error stop 11
	end if
	if ( any(a .ne. [1]) ) then
		print *, a
		error stop 12
	end if
	if ( any(b .ne. [12]) ) then
		print *, b
		error stop 13
	end if
	
	cafa = cafa - 5.5
	write(*, '(F6.2)') cafa
	sync all
	fun1 = 1
end function

