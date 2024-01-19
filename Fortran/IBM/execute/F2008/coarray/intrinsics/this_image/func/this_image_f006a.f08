!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : July 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : This_image() should produce consistent results
!*				 throughout the program.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

	integer, save :: caf[*]
	integer :: count, num

	interface
                subroutine check_results(c)
                        integer :: c[*]
                end subroutine
	end interface

	if (this_image() == 1) then
		caf[1] = this_image() * 100
	else if (this_image() == 2) then
		caf[2] = this_image() - 12
	else if (this_image() == 3) then
                caf[3] = this_image() + 17
	else
		caf = this_image() * 10
	end if

	call check_results(caf)

end


subroutine check_results(caf)
	integer :: caf[*]

	if (this_image() == 1) then
		if (caf[this_image()] /= this_image() * 100) then
                	print *, caf[1]
			error stop 11
		end if
        else if (this_image() == 2) then
		if (caf[this_image()] /= this_image() - 12) then
                        print *, caf[2]
                        error stop 12
                end if
        else if (this_image() == 3) then
		if (caf[this_image()] /= this_image() + 17) then
                        print *, caf[3]
                        error stop 13
                end if
        else
		if (caf /= this_image() * 10) then
                        print *, caf, this_image() * 10
                        error stop 14
                end if
        end if

end subroutine

