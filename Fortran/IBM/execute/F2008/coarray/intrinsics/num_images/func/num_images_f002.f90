!234567890123456789012345678901234567890123456789012345678901234567890
!*  ===================================================================
!*
!*  DATE                       : July 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Coarray
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Num_images produces a value matching the value
!*                               set by -qcaf=images sub-option.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
	integer, save :: caf[*]
	integer, parameter :: val = 6
	integer :: num(2)

	num(1) = num_images()
	num(2) = fun1()

	if (num(1) /= val) then
       	        print *, num(1)
               	error stop 11
        end if
	if (num(2) /= val) then
                print *, num(2)
                error stop 12
        end if

	caf = num_images()
	if (caf /= val) then
		print *, i, val, caf
		error stop 13
	end if

	sync all
contains

	integer function fun1()
		fun1 = num_images()
	end function

end
